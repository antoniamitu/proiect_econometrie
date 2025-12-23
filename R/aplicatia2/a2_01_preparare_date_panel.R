# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 2: DATE DE TIP PANEL
#
# SCRIPT: a2_01_preparare_date_panel.R
# RESPONSABIL: Studentul 1 (Data & Methodology)
#
# Ce face:
#  - verifica cheile iso_code + year (fara dubluri)
#  - trateaza lipsuri / outlieri (documentat)
#  - aplica transformari (log/Δ) doar daca are sens (flags OFF by default)
#  - sanity checks: corelatii + VIF pooled (orientativ)
# ==============================================================================

# 1) Setup si pachete ----------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor"))   install.packages("janitor")
if (!require("readr"))     install.packages("readr")
if (!require("here"))      install.packages("here")
if (!require("car"))       install.packages("car")

library(tidyverse)
library(janitor)
library(readr)
library(here)
library(car)

options(scipen = 999)

# 2) Paths (A2 in folderele principale: data/ + output/ + R/) ------------------

ROOT <- here()  # root-ul proiectului (unde e README.md)

# INPUT
DIR_RAW <- file.path(ROOT, "data", "raw")

# OUTPUT (A2 = sufix _2)
DIR_PROCESSED2 <- file.path(ROOT, "data", "processed_2")

DIR_OUTPUT   <- file.path(ROOT, "output")
DIR_TABLES2  <- file.path(DIR_OUTPUT, "tables_2")
DIR_TESTS2   <- file.path(DIR_OUTPUT, "tests_2")
DIR_MODELS2  <- file.path(DIR_OUTPUT, "models_2")
DIR_FIGURES2 <- file.path(DIR_OUTPUT, "figures_2")
DIR_PRED2    <- file.path(DIR_OUTPUT, "predictions_2")

dir.create(DIR_RAW,        recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PROCESSED2, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TABLES2,    recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TESTS2,     recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MODELS2,    recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_FIGURES2,   recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PRED2,      recursive = TRUE, showWarnings = FALSE)

# 2.1) Decision log (pentru LIVE + raport) -------------------------------------
LOG_FILE <- file.path(DIR_TESTS2, "decisions_log_2.md")

log_decision <- function(text) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("- [", ts, "] ", text, "\n"), file = LOG_FILE, append = TRUE)
}

if (!file.exists(LOG_FILE)) {
  cat("# Decisions log — Aplicația 2 (Studentul 1)\n\n", file = LOG_FILE)
  log_decision("Creat decisions log pentru A2.")
}

# 3) Import date ---------------------------------------------------------------
DATA_FILE <- file.path(DIR_RAW, "panel_2013_2023_FINAL.csv")

if (!file.exists(DATA_FILE)) {
  stop("Nu gasesc fisierul: ", DATA_FILE,
       "\nPune-l in data/raw/ (root), apoi ruleaza din nou.")
}

df_raw <- read_csv(DATA_FILE, show_col_types = FALSE) %>%
  clean_names()

log_decision(paste0("Import date: ", basename(DATA_FILE),
                    " | dimensiune = ", nrow(df_raw), " x ", ncol(df_raw), "."))

# 4) Chei panel + selectie/standardizare variabile -----------------------------
# Presupunere (conform variantelor voastre anterioare): iso_code + year exista.
# Daca in acest CSV cheile au alt nume, schimba aici.
KEY_ID   <- "iso_code"
KEY_TIME <- "year"

stopifnot(KEY_ID %in% names(df_raw))
stopifnot(KEY_TIME %in% names(df_raw))

# IMPORTANT: daca unele coloane nu exista in CSV-ul final, scriptul va da eroare.
# In acest caz, imi zici numele coloanelor si ajustam transmute-ul.
df0 <- df_raw %>%
  transmute(
    year = as.integer(.data[[KEY_TIME]]),
    iso_code = as.character(.data[[KEY_ID]]),
    country  = as.character(country),
    
    # Variabile model (standardizate ca Y, X1..X5)
    Y  = as.numeric(deposits_gdp),
    X1 = as.numeric(branches_100k),
    X2 = as.numeric(atms_100k),
    X3 = as.numeric(legal_rights),
    X4 = as.numeric(regulation),
    X5 = as.numeric(gdp_ppp),
    
    # Dummy 0/1 -> factor
    D = factor(high_freedom, levels = c(0, 1),
               labels = c("LowFreedom", "HighFreedom"))
  ) %>%
  arrange(iso_code, year)

log_decision("Standardizare variabile: Y=deposits_gdp; X1=branches_100k; X2=atms_100k; X3=legal_rights; X4=regulation; X5=gdp_ppp; D=high_freedom (factor).")

# 5) Verificare dubluri pe chei (iso_code-year) --------------------------------
dup_keys <- df0 %>%
  count(.data[[KEY_ID]], .data[[KEY_TIME]]) %>%
  filter(n > 1)

if (nrow(dup_keys) > 0) {
  log_decision("ATENTIE: dubluri pe chei (iso_code, year). Se aplica distinct() pe chei, pastrata prima observatie.")
  df0 <- df0 %>%
    distinct(.data[[KEY_ID]], .data[[KEY_TIME]], .keep_all = TRUE)
} else {
  log_decision("Verificare chei: NU exista dubluri pe (iso_code, year).")
}

# 6) Lipsuri: raport + regula --------------------------------------------------
na_report <- df0 %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  arrange(desc(na_count))

write_csv(na_report, file.path(DIR_TABLES2, "na_report_2.csv"))

n_before <- nrow(df0)
df1 <- df0 %>% drop_na()
n_after <- nrow(df1)

log_decision(paste0("Missing values: drop_na() pe toate variabilele selectate. Observatii: ", n_before, " -> ", n_after, "."))

# 7) Outlieri: raport documentat (IQR 1.5*IQR) ---------------------------------
# Nu “taiem” automat; doar raportam. Optional winsor (OFF by default).
num_vars <- df1 %>%
  select(where(is.numeric)) %>%
  names()

num_vars <- setdiff(num_vars, "year")  # year nu are sens ca outlier

outlier_iqr_count <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 5) return(NA_integer_)
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr
  hi <- q3 + 1.5 * iqr
  sum(x < lo | x > hi)
}

outliers_report <- tibble(variable = num_vars) %>%
  mutate(outliers_iqr = map_int(variable, ~ outlier_iqr_count(df1[[.x]])),
         n_non_missing = map_int(variable, ~ sum(!is.na(df1[[.x]]))),
         share_outliers = outliers_iqr / n_non_missing) %>%
  arrange(desc(share_outliers))

write_csv(outliers_report, file.path(DIR_TABLES2, "outliers_report_2.csv"))
log_decision("Outlieri: generat raport IQR (1.5*IQR) in output/tables_2/outliers_report_2.csv. Nu s-au aplicat corectii automat.")

# OPTIONAL: winsorizare (ramane OFF pana decideti explicit in live)
APPLY_WINSOR <- FALSE
WINSOR_P <- 0.01

winsorize <- function(x, p = 0.01) {
  q <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

if (APPLY_WINSOR) {
  df1 <- df1 %>%
    mutate(across(all_of(num_vars), ~ winsorize(.x, p = WINSOR_P)))
  log_decision(paste0("Aplicata winsorizare p=", WINSOR_P, " pe variabile numerice (exceptand year)."))
}

# 8) Transformari (log / delta) — OFF by default -------------------------------
# Cand decideti, se noteaza automat in log.
APPLY_LOG   <- FALSE
APPLY_DELTA <- FALSE

safe_log <- function(x) ifelse(is.na(x), NA_real_, ifelse(x > 0, log(x), NA_real_))

# Sugestie tipica: log pe GDP PPP (X5). Puteti adauga si Y daca e mereu pozitiv.
LOG_VARS <- c("X5")

# Sugestie tipica: delta in interiorul tarii (d_Y etc.) doar daca folositi dinamica
DELTA_VARS <- c("Y", "X1", "X2")

if (APPLY_LOG) {
  df1 <- df1 %>%
    mutate(across(all_of(LOG_VARS), safe_log, .names = "log_{.col}"))
  log_decision(paste0("Transformare: adaugate variabile log_ pentru: ", paste(LOG_VARS, collapse = ", "), "."))
}

if (APPLY_DELTA) {
  df1 <- df1 %>%
    group_by(iso_code) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(across(all_of(DELTA_VARS), ~ .x - lag(.x), .names = "d_{.col}")) %>%
    ungroup()
  log_decision(paste0("Transformare: adaugate variabile delta (d_) pentru: ", paste(DELTA_VARS, collapse = ", "), " in interiorul fiecarei tari."))
}

# 9) Sanity checks: descriptives + corelatii + VIF pooled ----------------------

# 9.1) Descriptives
desc_tbl <- df1 %>%
  select(-country, -iso_code, -D) %>%
  summarise(across(
    where(is.numeric),
    list(
      n = ~ sum(!is.na(.x)),
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      min = ~ min(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("variable", "stat"), sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(variable)

write_csv(desc_tbl, file.path(DIR_TABLES2, "descriptives_2.csv"))
log_decision("Export descriptives: output/tables_2/descriptives_2.csv.")

# 9.2) Corelatii (numeric, fara year)
num_for_cor <- df1 %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  names()

cor_long <- df1 %>%
  select(all_of(num_for_cor)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr")

write_csv(cor_long, file.path(DIR_TABLES2, "correlations_2.csv"))
log_decision("Export corelatii: output/tables_2/correlations_2.csv (numeric, fara year).")

# 9.3) VIF pooled (orientativ) — pe specificatia de baza
# Y ~ X1+X2+X3+X4+X5 + D
vif_formula <- Y ~ X1 + X2 + X3 + X4 + X5 + D
lm_fit <- lm(vif_formula, data = df1)

vif_vals <- car::vif(lm_fit)

vif_tbl <- if (is.matrix(vif_vals)) {
  tibble(term = rownames(vif_vals), vif = as.numeric(vif_vals[, 1]))
} else {
  tibble(term = names(vif_vals), vif = as.numeric(vif_vals))
}

vif_tbl <- vif_tbl %>% arrange(desc(vif))
write_csv(vif_tbl, file.path(DIR_TABLES2, "vif_pooled_2.csv"))
log_decision("Export VIF pooled (orientativ): output/tables_2/vif_pooled_2.csv pentru formula Y ~ X1+X2+X3+X4+X5 + D.")

# 10) Check panel: balanced/unbalanced (orientativ) ----------------------------
panel_counts <- df1 %>%
  count(iso_code, name = "T") %>%
  summarise(
    N_units = n(),
    T_min = min(T),
    T_max = max(T),
    balanced = (T_min == T_max)
  )

write_csv(panel_counts, file.path(DIR_TABLES2, "panel_balance_2.csv"))
log_decision(paste0("Panel check: N_units=", panel_counts$N_units,
                    ", T_min=", panel_counts$T_min,
                    ", T_max=", panel_counts$T_max,
                    ", balanced=", panel_counts$balanced, "."))

# 11) Export final (DOAR *_2) -------------------------------------------------
saveRDS(df1, file.path(DIR_PROCESSED2, "panel_final_2.rds"))
write_csv(df1, file.path(DIR_PROCESSED2, "panel_final_2.csv"))

log_decision("Export final: data/processed_2/panel_final_2.rds si panel_final_2.csv.")
message("GATA: A2_01 complet. Vezi data/processed_2/ si output/tables_2/ + output/tests_2/decisions_log_2.md.")
