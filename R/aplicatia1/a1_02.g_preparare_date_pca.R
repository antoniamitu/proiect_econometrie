# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: PREPARARE SETURI DE DATE PCA (PENTRU ML)
#
# SCRIPT: a1_02.g_preparare_date_pca.R
# RESPONSABIL: Student 3 (Data Scientist)
#
# OBIECTIVE:
# 1. Calcularea PCA doar pe setul de TRAIN (pentru a evita data leakage).
# 2. Proiectarea setului de TEST pe aceleasi componente principale.
# 3. Salvarea fisierelor gata de utilizare pentru modele (PCR, etc.).
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")

library(tidyverse)
library(caret)

# Output directories
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

# 2. Importarea Datelor Brute -------------------------------------------------
train_raw <- read.csv("data/processed/train_data_raw.csv")
test_raw  <- read.csv("data/processed/test_data_raw.csv")

# 3. Functie de Preprocesare (Logaritmare) -----------------------------------
# Aplicam aceeasi logaritmare validata in a1_04 (Modelul Optim)
preprocess_pca <- function(df) {
  df %>%
    mutate(
      l_Y  = log(Y),
      l_X1 = log(X1),
      l_X2 = log(X2),
      l_X5 = log(X5),
      D    = as.factor(D)   # 🔹 recomandat: asiguram D ca factor
    ) %>%
    select(iso_code, country, l_Y, l_X1, l_X2, X3, X4, l_X5, D)
}

train_log <- preprocess_pca(train_raw)
test_log  <- preprocess_pca(test_raw)

# 4. Calcularea PCA pe Setul de ANTRENARE ------------------------------------
vars_for_pca <- c("l_X1", "l_X2", "X3", "X4", "l_X5")

pca_model <- preProcess(
  train_log[, vars_for_pca],
  method = c("center", "scale", "pca"),
  thresh = 0.95
)

print(pca_model)

# 5. Aplicarea Transformarii (Proiectia) -------------------------------------

# TRAIN
train_pca_components <- predict(pca_model, train_log[, vars_for_pca])

train_data_pca <- bind_cols(
  train_log %>% select(iso_code, country, l_Y, D),
  train_pca_components
)

# TEST (folosind ACELASI pca_model)
test_pca_components <- predict(pca_model, test_log[, vars_for_pca])

test_data_pca <- bind_cols(
  test_log %>% select(iso_code, country, l_Y, D),
  test_pca_components
)

# 6. Salvarea Datelor PCA -----------------------------------------------------
write.csv(train_data_pca, "data/processed/train_data_pca.csv", row.names = FALSE)
write.csv(test_data_pca,  "data/processed/test_data_pca.csv",  row.names = FALSE)

# FULL – doar pentru vizualizari
full_raw <- read.csv("data/processed/full_data_raw.csv")
full_log <- preprocess_pca(full_raw)
full_pca_comp <- predict(pca_model, full_log[, vars_for_pca])

full_data_pca <- bind_cols(
  full_log %>% select(iso_code, country, l_Y, D),
  full_pca_comp
)

write.csv(full_data_pca, "data/processed/full_data_pca.csv", row.names = FALSE)

# 7. Mesaj de confirmare ------------------------------------------------------
cat("\n=======================================================\n")
cat(" DATELE PCA AU FOST GENERATE CU SUCCES!\n")
cat(" Fisiere salvate in 'data/processed/':\n")
cat(" 1. train_data_pca.csv (Pt antrenare modele PCR/Ridge/LASSO)\n")
cat(" 2. test_data_pca.csv  (Pt evaluare modele)\n")
cat(" 3. full_data_pca.csv  (Pt grafice / explorare)\n")
cat(" Numar componente principale generate:", ncol(train_pca_components), "\n")
cat("=======================================================\n")