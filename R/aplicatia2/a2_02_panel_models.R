# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 2: DATE DE TIP PANEL
#
# SCRIPT: a2_02_panel_models.R
# RESPONSABIL: Studentul 2 (Econometrie Panel)
#
# Ce face:
#  - incarca data/processed_2/panel_final_2.rds
#  - ruleaza: pooled OLS, FE, RE (optional: two-way FE)
#  - teste: F-test (pooled vs FE), LM (pooled vs RE), Hausman (FE vs RE)
#  - alege modelul final (regula standard pe baza testelor) + SE robuste
#    (minim: cluster pe tara / iso_code)
#  - optional: diagnostice (hetero/autocorelare/cross-section dependence)
#
# Exporta (DOAR *_2, in folderele principale):
#  - output/models_2/model_final_2.rds
#  - output/tables_2/model_summary_2.csv
#  - output/tables_2/tests_2.csv
#  - (optional) output/tables_2/diagnostics_2.csv
#  - completeaza output/tests_2/decisions_log_2.md
# ==============================================================================

# 1) Setup si pachete ----------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("here"))      install.packages("here")
if (!require("plm"))       install.packages("plm")
if (!require("lmtest"))    install.packages("lmtest")
if (!require("sandwich"))  install.packages("sandwich")
if (!require("broom"))     install.packages("broom")

library(tidyverse)
library(here)
library(plm)
library(lmtest)
library(sandwich)
library(broom)

options(scipen = 999)

# 2) Paths (A2 in folderele principale: data/ + output/ + R/) ------------------
ROOT <- here()

DIR_PROCESSED2 <- file.path(ROOT, "data", "processed_2")

DIR_OUTPUT   <- file.path(ROOT, "output")
DIR_TABLES2  <- file.path(DIR_OUTPUT, "tables_2")
DIR_TESTS2   <- file.path(DIR_OUTPUT, "tests_2")
DIR_MODELS2  <- file.path(DIR_OUTPUT, "models_2")

dir.create(DIR_PROCESSED2, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TABLES2,    recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TESTS2,     recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_MODELS2,    recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(DIR_TESTS2, "decisions_log_2.md")
log_decision <- function(text) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("- [", ts, "] ", text, "\n"), file = LOG_FILE, append = TRUE)
}
if (!file.exists(LOG_FILE)) {
  cat("# Decisions log — Aplicația 2 (Studentul 2)\n\n", file = LOG_FILE)
  log_decision("Creat decisions log pentru A2 (Studentul 2).")
}

# 3) Incarcare date ------------------------------------------------------------
DATA_RDS <- file.path(DIR_PROCESSED2, "panel_final_2.rds")
stopifnot(file.exists(DATA_RDS))

df <- readRDS(DATA_RDS)

log_decision(paste0("Incarcat dataset: data/processed_2/panel_final_2.rds | dimensiune = ",
                    nrow(df), " x ", ncol(df), "."))

# 4) Definire panel + formula --------------------------------------------------
# Chei (conform a2_01): iso_code + year
stopifnot(all(c("iso_code", "year") %in% names(df)))

pdf <- plm::pdata.frame(df, index = c("iso_code", "year"))

# Specificatie baza (ajustezi doar aici daca e nevoie)
# D e factor (LowFreedom/HighFreedom) deja in date.
formula_base <- Y ~ X1 + X2 + X3 + X4 + X5 + D

log_decision(paste0("Formula baza folosita: ", deparse(formula_base)))

# Optional: two-way FE (tara + an)
USE_TWOWAY_FE <- FALSE

# 5) Modele: pooled, FE, RE (si optional two-way FE) ---------------------------
m_pool <- plm(formula_base, data = pdf, model = "pooling")

m_fe   <- plm(formula_base, data = pdf, model = "within", effect = "individual")
m_re   <- plm(formula_base, data = pdf, model = "random", effect = "individual")

m_fe_tw <- NULL
if (USE_TWOWAY_FE) {
  m_fe_tw <- plm(formula_base, data = pdf, model = "within", effect = "twoways")
  log_decision("Rulat si model FE two-way (individual + time).")
}

# 6) Teste selectie model ------------------------------------------------------
# 6.1 F-test: pooled vs FE
test_F <- pFtest(m_fe, m_pool)

# 6.2 LM test (Breusch-Pagan): pooled vs RE
# tip "bp" = Breusch-Pagan LM
test_LM <- plmtest(m_pool, type = "bp")

# 6.3 Hausman: FE vs RE
test_H <- phtest(m_fe, m_re)

tests_tbl <- tibble(
  test = c("F-test (pooled vs FE)", "LM BP (pooled vs RE)", "Hausman (FE vs RE)"),
  statistic = c(as.numeric(test_F$statistic), as.numeric(test_LM$statistic), as.numeric(test_H$statistic)),
  df = c(
    paste(test_F$parameter, collapse = ", "),
    paste(test_LM$parameter, collapse = ", "),
    paste(test_H$parameter, collapse = ", ")
  ),
  p_value = c(test_F$p.value, test_LM$p.value, test_H$p.value)
)

write_csv(tests_tbl, file.path(DIR_TABLES2, "tests_2.csv"))
log_decision("Export teste selectie model: output/tables_2/tests_2.csv (F, LM, Hausman).")

# 7) Regula de alegere a modelului final --------------------------------------
# Regula standard:
# - daca F semnificativ -> FE vs pooled
# - daca LM semnificativ -> RE vs pooled
# - daca ambele sugereaza efecte, folosim Hausman:
#     p<0.05 -> FE, altfel -> RE
# - daca niciunul nu e semnificativ -> pooled
alpha <- 0.05

F_sig  <- test_F$p.value  < alpha
LM_sig <- test_LM$p.value < alpha
H_sig  <- test_H$p.value  < alpha

chosen_model_name <- NA_character_
chosen_model <- NULL

if (!F_sig && !LM_sig) {
  chosen_model_name <- "Pooled OLS (pooling)"
  chosen_model <- m_pool
} else if (F_sig && !LM_sig) {
  chosen_model_name <- "Fixed Effects (FE, individual)"
  chosen_model <- m_fe
} else if (!F_sig && LM_sig) {
  chosen_model_name <- "Random Effects (RE, individual)"
  chosen_model <- m_re
} else {
  # ambele sugereaza efecte -> Hausman
  if (H_sig) {
    chosen_model_name <- "Fixed Effects (FE, individual) [Hausman p<0.05]"
    chosen_model <- m_fe
  } else {
    chosen_model_name <- "Random Effects (RE, individual) [Hausman p>=0.05]"
    chosen_model <- m_re
  }
}

log_decision(paste0("Alegere model final (alpha=", alpha, "): ",
                    chosen_model_name, " | p(F)=", round(test_F$p.value, 4),
                    ", p(LM)=", round(test_LM$p.value, 4),
                    ", p(H)=", round(test_H$p.value, 4), "."))

# Daca ai pornit two-way FE si vrei sa-l impui manual ca final:
# chosen_model_name <- "Fixed Effects two-way (individual+time)"
# chosen_model <- m_fe_tw

# 8) Erori standard robuste (minim: cluster pe tara) ---------------------------
# Pentru panel, cluster pe grup (iso_code) este cerinta minima.
# vcovHC cu cluster="group" (Arellano) e o alegere standard.
vcov_cluster <- function(model) {
  sandwich::vcovHC(model, type = "HC1", cluster = "group")
}

ct_robust <- lmtest::coeftest(chosen_model, vcov. = vcov_cluster(chosen_model))

# Construim tabel “curat” cu coeficienti + SE robuste
model_summary <- tibble(
  term = rownames(ct_robust),
  estimate = ct_robust[, 1],
  std_error = ct_robust[, 2],
  statistic = ct_robust[, 3],
  p_value = ct_robust[, 4]
) %>%
  mutate(
    model = chosen_model_name,
    alpha = alpha
  )

write_csv(model_summary, file.path(DIR_TABLES2, "model_summary_2.csv"))
log_decision("Export model summary (SE robuste cluster pe tara): output/tables_2/model_summary_2.csv.")

# 9) (Optional) Diagnostice reziduuri ------------------------------------------
RUN_DIAGNOSTICS <- TRUE

if (RUN_DIAGNOSTICS) {
  # Autocorelare (panel BG test)
  # NOTA: pbgtest are sens pt FE/RE; pentru pooled poate fi mai putin relevant
  diag_tbl <- tibble(
    test = character(),
    statistic = double(),
    df = character(),
    p_value = double()
  )
  
  # Heteroscedasticitate (Breusch-Pagan) pe plm: bptest(plm_model)
  # Pentru plm, bptest vine din lmtest si accepta formula/model.
  # Folosim versiunea pe model plm.
  try({
    t_bptest <- lmtest::bptest(chosen_model)
    diag_tbl <- bind_rows(diag_tbl, tibble(
      test = "BP heteroscedasticity (bptest)",
      statistic = as.numeric(t_bptest$statistic),
      df = paste(t_bptest$parameter, collapse = ", "),
      p_value = t_bptest$p.value
    ))
  }, silent = TRUE)
  
  # Autocorelare panel (BG)
  try({
    t_pbg <- plm::pbgtest(chosen_model)
    diag_tbl <- bind_rows(diag_tbl, tibble(
      test = "Panel autocorrelation (pbgtest)",
      statistic = as.numeric(t_pbg$statistic),
      df = paste(t_pbg$parameter, collapse = ", "),
      p_value = t_pbg$p.value
    ))
  }, silent = TRUE)
  
  # Cross-sectional dependence (Pesaran CD)
  try({
    t_pcd <- plm::pcdtest(chosen_model, test = "cd")
    diag_tbl <- bind_rows(diag_tbl, tibble(
      test = "Cross-sectional dependence (pcdtest, Pesaran CD)",
      statistic = as.numeric(t_pcd$statistic),
      df = paste(t_pcd$parameter, collapse = ", "),
      p_value = t_pcd$p.value
    ))
  }, silent = TRUE)
  
  if (nrow(diag_tbl) > 0) {
    write_csv(diag_tbl, file.path(DIR_TABLES2, "diagnostics_2.csv"))
    log_decision("Export diagnostice: output/tables_2/diagnostics_2.csv.")
  } else {
    log_decision("Diagnostice: nu s-a putut genera niciun test (posibil model/conditii).")
  }
}

# 10) Salvare model final ------------------------------------------------------
saveRDS(chosen_model, file.path(DIR_MODELS2, "model_final_2.rds"))
log_decision("Salvat model final: output/models_2/model_final_2.rds.")

message("GATA: a2_02_panel_models.R complet. Vezi output/tables_2/ (tests_2, model_summary_2, optional diagnostics_2) + output/models_2/model_final_2.rds.")
