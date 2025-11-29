# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1 (VARIANTA FARA OUTLIERS)
# Rol: Student 2 (Econometricianul Clasic)
# Etapa: 3. Modelare Econometrică Clasică (OLS) - Date Curatate
# ==============================================================================

# 1. Incarcare biblioteci
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(car)) install.packages("car")
if(!require(lmtest)) install.packages("lmtest")
if(!require(sandwich)) install.packages("sandwich")
if(!require(readxl)) install.packages("readxl")

library(tidyverse)
library(caret)
library(car)
library(lmtest)
library(sandwich)
library(readxl)

# 2. Incarcare date
# Folosim skip=1 pentru a sari peste randul cu "Indicators"
df <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx", skip = 1)

# 3. ELIMINAREA OUTLIERILOR (Pas Nou)
# Eliminam tarile care au depozite peste 200% din PIB (Hong Kong, Singapore)
# Acestea distorsionau modelul anterior.
df_clean <- df %>%
  filter(deposits_gdp < 200) %>%
  select(deposits_gdp,       # Y
         branches_100k,      # X1
         atms_100k,          # X2
         legal_rights,       # X3
         regulation,         # X4
         gdp_pc_ppp,         # X5
         country) %>%        # Pastram si numele tarii pentru verificare
  na.omit()

cat("Numar observatii dupa eliminarea outlierilor:", nrow(df_clean), "\n")

# 4. Impartirea datelor (Train/Test)
set.seed(123)
train_index <- createDataPartition(df_clean$deposits_gdp, p = 0.8, list = FALSE)
data_train <- df_clean[train_index, ]
data_test  <- df_clean[-train_index, ]

# 5. Estimarea Modelului pe datele curate
model_ols_clean <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + regulation + gdp_pc_ppp, 
                      data = data_train)

# 6. Rezultate si Diagnostic
cat("\n=== REZUMAT MODEL (FARA OUTLIERS) ===\n")
summary(model_ols_clean)

cat("\n=== MULTICOLINIARITATE (VIF) ===\n")
print(vif(model_ols_clean))

cat("\n=== TEST HETEROSCEDASTICITATE (Breusch-Pagan) ===\n")
print(bptest(model_ols_clean))

# 7. Grafice de diagnostic
par(mfrow = c(2, 2))
plot(model_ols_clean, main = "Diagnostic Model Curat")
par(mfrow = c(1, 1))

# ==============================================================================
# PASUL 3.c: VALIDAREA PE SETUL DE TEST (Out-of-sample prediction)
# ==============================================================================

# 1. Facem predictii pe datele de test folosind modelul curatat
predictii_test <- predict(model_ols_clean, newdata = data_test)

# 2. Comparam valorile reale cu cele prezise
rezultate_test <- data.frame(
  Tara = data_test$country,
  Real = data_test$deposits_gdp,
  Prezis = predictii_test,
  Eroare = data_test$deposits_gdp - predictii_test
)

# 3. Calculam indicatorii de performanta (RMSE, MAE, R2)
# RMSE (Root Mean Squared Error) - cat gresim in medie
rmse_val <- sqrt(mean(rezultate_test$Eroare^2))
mae_val <- mean(abs(rezultate_test$Eroare))

# R2 pe setul de test
sse <- sum(rezultate_test$Eroare^2)
sst <- sum((data_test$deposits_gdp - mean(data_test$deposits_gdp))^2)
r2_test <- 1 - (sse / sst)

# 4. Afisarea rezultatelor
cat("\n=== PERFORMANTA PE SETUL DE TEST ===\n")
cat("RMSE (Eroarea medie): ", round(rmse_val, 4), "\n")
cat("MAE (Eroarea absoluta): ", round(mae_val, 4), "\n")
cat("R2 pe Test: ", round(r2_test, 4), "\n")