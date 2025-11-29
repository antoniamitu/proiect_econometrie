# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1
# Rol: Student 2 (Econometricianul Clasic)
# Etapa: 4. Extinderea Modelului si Scenarii de Prognoza
# Fisier: a1_04_model_extins_scenarii.R
# ==============================================================================

# 1. Verificare preliminara
# Ne asiguram ca avem datele originale incarcate (tabelul 'df')
if(!exists("df")) {
  stop("EROARE: Variabila 'df' nu exista. Te rog ruleaza mai intai scriptul 'a1_03_regresie_clasica.R'!")
}

# Incarcare biblioteci necesare
library(tidyverse)
library(caret)
library(lmtest)

# ==============================================================================
# PASUL 4.a: EXTINDEREA MODELULUI (Forme Neliniare & Interactiune)
# ==============================================================================

cat("\n--- 4.a EXTINDEREA MODELULUI ---\n")

# 1. Pregatim datele pentru modelul extins
# ATENTIE: Folosim 'df' (originalul), nu 'df_clean', pentru a avea acces la coloana 'high_freedom'
df_extins <- df %>%
  filter(deposits_gdp < 200) %>%  # Re-aplicam filtrul de outliers (fara HK/Singapore)
  mutate(high_freedom = as.numeric(high_freedom)) %>% # Ne asiguram ca e numeric
  mutate(high_freedom = ifelse(is.na(high_freedom), 
                               # Daca lipseste valoarea, o estimam: 1 daca Regulation > Mediana, altfel 0
                               ifelse(regulation > median(regulation, na.rm = TRUE), 1, 0), 
                               high_freedom)) %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, 
         regulation, gdp_pc_ppp, high_freedom, country) %>%
  na.omit()

# 2. Refacem impartirea train/test (pastrand aceeasi samanta 123)
set.seed(123)
train_index_ext <- createDataPartition(df_extins$deposits_gdp, p = 0.8, list = FALSE)
data_train_ext <- df_extins[train_index_ext, ]
data_test_ext  <- df_extins[-train_index_ext, ]

# 3. Estimam Modelul Extins
# Specificatii noi:
# - log(gdp_pc_ppp): Transformare neliniara
# - atms_100k * high_freedom: Interactiune
model_extins <- lm(deposits_gdp ~ branches_100k + legal_rights + regulation + 
                     log(gdp_pc_ppp) + atms_100k * high_freedom, 
                   data = data_train_ext)

# 4. Afisam Rezultatele
cat("\n=== REZUMAT MODEL EXTINS (Log & Interactiune) ===\n")
summary(model_extins)

# 5. Comparatie cu Modelul Anterior (AIC)
# Avem nevoie de modelul simplu pentru comparatie. Il re-estimam rapid aici daca nu exista.
if(!exists("model_ols_clean")) {
  # Reconstructie rapida a modelului simplu pe noile date (pentru comparatie corecta)
  model_ols_clean <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + regulation + gdp_pc_ppp, 
                        data = data_train_ext)
}

cat("\n=== COMPARATIE PERFORMANTA (AIC) ===\n")
cat("AIC Model Simplu (Linear): ", AIC(model_ols_clean), "\n")
cat("AIC Model Extins (Log+Int):", AIC(model_extins), "\n")

cat("R2 Ajustat (Simplu):", summary(model_ols_clean)$adj.r.squared, "\n")
cat("R2 Ajustat (Extins):", summary(model_extins)$adj.r.squared, "\n")

# ==============================================================================
# PASUL 4.b: SCENARII DE PROGNOZA (Pe Modelul Optim - Cel Simplu)
# ==============================================================================

cat("\n--- 4.b SCENARII DE PROGNOZA ---\n")

# 1. Extragem datele actuale pentru Romania
date_romania <- df_extins %>% 
  filter(country == "Romania")

cat("\n=== DATE ACTUALE ROMANIA ===\n")
print(date_romania)

# 2. Definim Scenariul: "Dublarea numarului de sucursale"
scenariu_romania <- date_romania
scenariu_romania$branches_100k <- date_romania$branches_100k * 2

cat("\n=== SCENARIU: DUBLARE SUCURSALE (Branches) ===\n")
cat("Valoare Actuala Branches:", date_romania$branches_100k, "\n")
cat("Valoare Scenariu Branches:", scenariu_romania$branches_100k, "\n")

# 3. Calculam Prognoza folosind MODELUL SIMPLU (fiind cel optim)
prognoza <- predict(model_ols_clean, 
                    newdata = scenariu_romania, 
                    interval = "confidence", 
                    level = 0.95)

# 4. Afisarea Rezultatului Economic
cat("\n=== REZULTAT PROGNOZA DEPOZITE (% PIB) ===\n")
cat("Nivel Actual Depozite (Real):    ", date_romania$deposits_gdp, "%\n")
cat("Nivel Prognozat (in Scenariu):   ", round(prognoza[1], 2), "%\n")
cat("Impact estimat (Crestere):       +", round(prognoza[1] - date_romania$deposits_gdp, 2), " puncte procentuale\n")

# 5. Salvarea rezultatelor
sink("output/rezultate_student2_scenarii.txt")
cat("--- REZULTATE COMPARATIE MODELE ---\n")
cat("AIC Simplu:", AIC(model_ols_clean), " vs AIC Extins:", AIC(model_extins), "\n")
cat("\n--- PROGNOZA ROMANIA ---\n")
print(prognoza)
sink()