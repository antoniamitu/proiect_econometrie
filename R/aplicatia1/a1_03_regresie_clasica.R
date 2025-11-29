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

# ==============================================================================
# PASUL 4.a: EXTINDEREA MODELULUI (Forme Neliniare & Interactiune)
# ==============================================================================

# 1. Pregatim datele pentru modelul extins
# Ne asiguram ca variabila 'high_freedom' exista. 
# Daca este goala in Excel, o calculam noi pe baza mediei 'regulation' (proxy pentru libertate)
df_extins <- df %>%
  filter(deposits_gdp < 200) %>%  # Pastram filtrul de outliers (FARA Hong Kong/Singapore)
  mutate(high_freedom = ifelse(is.na(high_freedom), 
                               ifelse(regulation > median(regulation, na.rm = TRUE), 1, 0), 
                               high_freedom)) %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, 
         regulation, gdp_pc_ppp, high_freedom, country) %>%
  na.omit()

# 2. Refacem impartirea train/test (pastrand aceeasi samanta 123 pentru comparabilitate)
set.seed(123)
train_index_ext <- createDataPartition(df_extins$deposits_gdp, p = 0.8, list = FALSE)
data_train_ext <- df_extins[train_index_ext, ]
data_test_ext  <- df_extins[-train_index_ext, ]

# 3. Estimam Modelul Extins
# Specificatii noi:
# - log(gdp_pc_ppp): Transformam PIB-ul pentru a capta relatia neliniara
# - atms_100k * high_freedom: Termen de interactiune (testam daca ATM-urile conteaza mai mult in tarile libere)
model_extins <- lm(deposits_gdp ~ branches_100k + legal_rights + regulation + 
                     log(gdp_pc_ppp) + atms_100k * high_freedom, 
                   data = data_train_ext)

# 4. Afisam Rezultatele
cat("\n=== REZUMAT MODEL EXTINS (Log & Interactiune) ===\n")
summary(model_extins)

# 5. Comparatie cu Modelul Anterior (AIC)
# AIC (Akaike Information Criterion) masoara calitatea modelului. 
# O valoare MAI MICA inseamna un model MAI BUN.
cat("\n=== COMPARATIE PERFORMANTA (AIC) ===\n")
cat("AIC Model Simplu (Linear): ", AIC(model_ols_clean), "\n")
cat("AIC Model Extins (Log+Int):", AIC(model_extins), "\n")

# Verificam daca R-squared a crescut
cat("R2 Ajustat (Simplu):", summary(model_ols_clean)$adj.r.squared, "\n")
cat("R2 Ajustat (Extins):", summary(model_extins)$adj.r.squared, "\n")

# ==============================================================================
# PASUL 4.b: SCENARII DE PROGNOZA (Pe Modelul Optim - Cel Simplu)
# ==============================================================================

# 1. Extragem datele actuale pentru Romania
# (Ne asiguram ca Romania exista in setul de date curat)
date_romania <- df_clean %>% 
  filter(country == "Romania")

cat("\n=== DATE ACTUALE ROMANIA ===\n")
print(date_romania)

# 2. Definim Scenariul: "Dublarea numarului de sucursale"
# Ipoteza: Bancile investesc masiv in prezenta fizica (branches_100k se dubleaza)
# Restul variabilelor (PIB, Reglementare etc.) raman constante (ceteris paribus)
scenariu_romania <- date_romania
scenariu_romania$branches_100k <- date_romania$branches_100k * 2

cat("\n=== SCENARIU: DUBLARE SUCURSALE (Branches) ===\n")
cat("Valoare Actuala Branches:", date_romania$branches_100k, "\n")
cat("Valoare Scenariu Branches:", scenariu_romania$branches_100k, "\n")

# 3. Calculam Prognoza
# Folosim 'interval = "confidence"' pentru a vedea si intervalul de incredere 95%
prognoza <- predict(model_ols_clean, 
                    newdata = scenariu_romania, 
                    interval = "confidence", 
                    level = 0.95)

# 4. Afisarea Rezultatului Economic
cat("\n=== REZULTAT PROGNOZA DEPOZITE (% PIB) ===\n")
cat("Nivel Actual Depozite (Real):    ", date_romania$deposits_gdp, "%\n")
cat("Nivel Prognozat (in Scenariu):   ", round(prognoza[1], 2), "%\n")
cat("Impact estimat (Crestere):       +", round(prognoza[1] - date_romania$deposits_gdp, 2), " puncte procentuale\n")

# 5. Salvarea tuturor rezultatelor importante intr-un fisier text (Optional dar util)
sink("output/rezultate_student2.txt")
cat("--- REZULTATE MODEL OLS ---\n")
summary(model_ols_clean)
cat("\n--- DIAGNOSTIC ---\n")
print(bptest(model_ols_clean))
cat("\n--- PROGNOZA ROMANIA ---\n")
print(prognoza)
sink() # Opreste scrierea in fisier
cat("\n(Rezultatele au fost salvate si in 'output/rezultate_student2.txt')\n")