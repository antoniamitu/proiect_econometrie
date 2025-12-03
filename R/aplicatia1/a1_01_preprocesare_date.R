# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: ITERATIA 1 - DATE BRUTE (BASELINE)
#
# SCRIPT: a1_01_preprocesare_date.R
# RESPONSABIL: Student 3 (Data Scientist)
#
# SCOP: Pregatirea datelor EXACT asa cum au venit (fara logaritmi/standardizare)
#       pentru a demonstra ulterior necesitatea transformarilor.
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret") 
if (!require("stargazer")) install.packages("stargazer")
if (!require("readxl")) install.packages("readxl")

library(tidyverse)
library(caret)
library(stargazer)
library(readxl)

# 2. Importarea Datelor -------------------------------------------------------
df_raw <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx")

# Verificare rapida a structurii
str(df_raw)
head(df_raw)

# 3. Selectia si Redenumirea Variabilelor ------------------------------------
# Conform fisierului Indicatori.txt / designul proiectului
df_clean <- df_raw %>%
  select(
    iso_code,
    country,
    Y  = deposits_gdp,   # Variabila dependenta
    X1 = branches_100k,  # Infrastructura fizica
    X2 = atms_100k,      # Infrastructura digitala
    X3 = legal_rights,   # Institutional
    X4 = regulation,     # Institutional
    X5 = gdp_pc_ppp,     # Control (PIB/capita)
    D  = high_freedom    # Dummy (0/1)
  ) %>%
  mutate(
    across(c(Y, X1, X2, X3, X4, X5), as.numeric),
    D = factor(D, levels = c(0,1),
               labels = c("LowFreedom", "HighFreedom"))
  )

# 4. Tratarea Valorilor Lipsa (Missing Values) -------------------------------
# Eliminam randurile cu NA pentru a avea un set de date consistent
df_final <- df_clean %>%
  drop_na()

cat("Observatii initiale: ", nrow(df_clean), "\n")
cat("Observatii ramase dupa eliminarea NA:", nrow(df_final), "\n")

# 5. Impartirea Train / Test (80% / 20%) -------------------------------------
set.seed(123) # Foarte important pentru reproductibilitate!

# Indexul se face pe baza lui Y pentru a pastra distributia
train_index <- createDataPartition(df_final$Y, p = 0.8, list = FALSE)

data_train <- df_final[train_index, ]
data_test  <- df_final[-train_index, ]

cat("Observatii in TRAIN:", nrow(data_train), "\n")
cat("Observatii in TEST :", nrow(data_test), "\n")

# 6. Exportarea Datelor "Brute" ----------------------------------------------
# Le salvam explicit ca fiind versiunea "raw" pentru a nu le confunda mai tarziu
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

write.csv(data_train, "data/processed/train_data_raw.csv", row.names = FALSE)
write.csv(data_test,  "data/processed/test_data_raw.csv",  row.names = FALSE)
write.csv(df_final,   "data/processed/full_data_raw.csv",  row.names = FALSE)

# 7. Statistici descriptive de baza ------------------------------------------
stargazer(
  df_final %>% select(-iso_code, -country, -D),
  type  = "text",
  title = "Statistici Descriptive - Date Brute (Baseline)"
)