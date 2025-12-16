# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 2: SETUP STRUCTURA PANEL
#
# SCRIPT: a2_01_setup_structura_panel.R
# RESPONSABIL: Student 1 (Data & Methodology)
#
# SCOP:
# 1) Import + curatare minima (transparenta)
# 2) Verificare chei panel (iso_code + year), fara dubluri
# 3) Raportare lipsuri (documentat) + sanity checks
# 4) Creare obiect panel (pdata.frame) pentru FE/RE
# 5) Export panel_final.rds pentru scripturile urmatoare
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("plm")) install.packages("plm")
if (!require("stargazer")) install.packages("stargazer")
if (!require("janitor")) install.packages("janitor")

library(tidyverse)
library(readr)
library(plm)
library(stargazer)
library(janitor)

# 2. Importarea Datelor -------------------------------------------------------
df_raw <- read_csv(
  "data/raw/panel_2013_2023_FINAL.csv",
  show_col_types = FALSE
)

cat("\nDimensiuni dataset:", nrow(df_raw), "observatii,", ncol(df_raw), "coloane\n")
glimpse(df_raw)

# 3. Curatare de baza ---------------------------------------------------------
df_clean <- df_raw %>%
  janitor::clean_names()

# 4. Definirea cheilor panel --------------------------------------------------
id_var   <- "iso_code"
time_var <- "year"

require_col <- function(df, colname) {
  if (!colname %in% names(df)) {
    stop(
      paste0("Lipseste coloana obligatorie: `", colname, "`.\n",
             "Coloane existente: ", paste(names(df), collapse = ", "))
    )
  }
}
require_col(df_clean, id_var)
require_col(df_clean, time_var)

df_clean <- df_clean %>%
  mutate(
    year = as.integer(year),
    iso_code = as.factor(iso_code),
    country = as.factor(country),
    high_freedom = as.integer(high_freedom) # 0/1
  )

# 5. Verificare dubluri pe cheie (iso_code, year) ------------------------------
dups <- df_clean %>%
  count(.data[[id_var]], .data[[time_var]], name = "n") %>%
  filter(n > 1)

cat("\n================ CHECK: DUBLURI CHEIE PANEL ================\n")
if (nrow(dups) == 0) {
  cat("OK: Nu exista dubluri pe cheie (", id_var, ", ", time_var, ").\n", sep = "")
} else {
  cat("ATENTIE: Exista dubluri pe cheie. Afisez primele 20:\n")
  print(dups %>% arrange(desc(n)) %>% head(20))
  df_clean <- df_clean %>%
    arrange(.data[[id_var]], .data[[time_var]]) %>%
    distinct(.data[[id_var]], .data[[time_var]], .keep_all = TRUE)
  cat("Actiune: am pastrat prima observatie pentru fiecare (id, year).\n")
}

# 6. Raport missingness (NU eliminam agresiv in setup) -------------------------
cat("\n================ CHECK: MISSINGNESS ================\n")
missing_report <- df_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  arrange(desc(na_count))

print(missing_report)

# Eliminam doar NA din chei (nu ar trebui sa existe, dar pastram disciplina)
n_before <- nrow(df_clean)
df_clean <- df_clean %>%
  filter(!is.na(.data[[id_var]]), !is.na(.data[[time_var]]))
n_after <- nrow(df_clean)
cat("\nObservatii initiale:", n_before, "| ramase dupa eliminarea NA pe chei:", n_after, "\n")

# 7. Creare obiect panel + verificare balanced/unbalanced ---------------------
pdata <- pdata.frame(df_clean, index = c(id_var, time_var), drop.index = FALSE, row.names = TRUE)

cat("\n================ STRUCTURA PANEL (pdim) ================\n")
print(pdim(pdata))

obs_per_id <- df_clean %>%
  count(.data[[id_var]], name = "n_obs") %>%
  arrange(desc(n_obs))

years_range <- range(df_clean[[time_var]], na.rm = TRUE)
cat("\nInterval ani:", years_range[1], "-", years_range[2], "\n")
cat("Numar unitati (", id_var, "):", n_distinct(df_clean[[id_var]]), "\n")
cat("Numar ani unici:", n_distinct(df_clean[[time_var]]), "\n")

# 8. Sanity checks minime: summary numerice -----------------------------------
cat("\n================ DESCRIPTIVE (numerice) - rapid ================\n")
stargazer(
  df_clean %>% select(where(is.numeric)),
  type  = "text",
  title = "Statistici descriptive rapide (numerice) - Panel Setup"
)

# 9. Export -------------------------------------------------------------------
if (!dir.exists("data/processed_2")) dir.create("data/processed_2", recursive = TRUE)
if (!dir.exists("output/tests_2")) dir.create("output/tests_2", recursive = TRUE)

saveRDS(pdata, "data/processed_2/panel_final.rds")
write_csv(missing_report, "output/tests_2/a2_01_missing_report_panel.csv")
write_csv(obs_per_id, "output/tests_2/a2_01_obs_per_unit_panel.csv")

cat("\nEXPORT OK:\n")
cat("- data/processed_2/panel_final.rds\n")
cat("- output/tests_2/a2_01_missing_report_panel.csv\n")
cat("- output/tests_2/a2_01_obs_per_unit_panel.csv\n")
