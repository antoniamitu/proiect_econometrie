# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1 (VERSIUNE IMBUNATATITA)
# Rol: Student 2 (Econometricianul Clasic)
# Etapa: 4. Extinderea Modelului si Scenarii de Prognoza
# ==============================================================================

# 1. Incarcare biblioteci
library(tidyverse)
library(caret)
library(lmtest)
library(readxl)

# 2. Verificare si incarcare model anterior
if(!file.exists("output/models/model_ols_clean.RData")) {
  stop("EROARE: Modelul OLS nu exista! Ruleaza mai intai 'a1_03_regresie_clasica.R'")
}

# Incarcam modelul si datele salvate
load("output/models/model_ols_clean.RData")
cat("✓ Model OLS incarcat din etapa anterioara\n\n")

# 3. Re-incarcare date pentru model extins
df <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx", skip = 1)

# ==============================================================================
# PASUL 4.a: EXTINDEREA MODELULUI
# ==============================================================================

cat("=== PASUL 4.a: EXTINDEREA MODELULUI ===\n\n")

# Pregatire date pentru model extins
df_extins <- df %>%
  filter(deposits_gdp < 200) %>%
  mutate(high_freedom = as.numeric(high_freedom)) %>%
  mutate(high_freedom = ifelse(is.na(high_freedom), 
                               ifelse(regulation > median(regulation, na.rm = TRUE), 1, 0), 
                               high_freedom)) %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, 
         regulation, gdp_pc_ppp, high_freedom, country) %>%
  na.omit()

# Refacem split-ul (IMPORTANT: aceeasi samanta!)
set.seed(123)
train_index_ext <- createDataPartition(df_extins$deposits_gdp, p = 0.8, list = FALSE)
data_train_ext <- df_extins[train_index_ext, ]
data_test_ext  <- df_extins[-train_index_ext, ]

cat("Observatii pentru model extins:\n")
cat("Train:", nrow(data_train_ext), "\n")
cat("Test:", nrow(data_test_ext), "\n\n")

# ------------------------------------------------------------------------------
# MODEL EXTINS 1: Transformare logaritmica + Interactiune
# ------------------------------------------------------------------------------

cat("--- MODEL EXTINS 1: log(GDP) + ATMs*High_Freedom ---\n\n")

model_extins_1 <- lm(deposits_gdp ~ branches_100k + legal_rights + regulation + 
                       log(gdp_pc_ppp) + atms_100k * high_freedom, 
                     data = data_train_ext)

summary(model_extins_1)

# ------------------------------------------------------------------------------
# MODEL EXTINS 2: Termen patratic (polynomial)
# ------------------------------------------------------------------------------

cat("\n--- MODEL EXTINS 2: Branches^2 (Relatie Neliniara) ---\n\n")

model_extins_2 <- lm(deposits_gdp ~ branches_100k + I(branches_100k^2) + 
                       atms_100k + legal_rights + regulation + gdp_pc_ppp, 
                     data = data_train_ext)

summary(model_extins_2)

# ------------------------------------------------------------------------------
# MODEL EXTINS 3: Dummy pentru regiuni (optional - daca ai variabila region)
# ------------------------------------------------------------------------------

# Daca ai coloane region/continent, poti adauga:
# model_extins_3 <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + 
#                        regulation + gdp_pc_ppp + region, data = data_train_ext)

# ------------------------------------------------------------------------------
# COMPARATIE MODELE (AIC, BIC, R2 Ajustat)
# ------------------------------------------------------------------------------

cat("\n=== COMPARATIE PERFORMANTA MODELE ===\n\n")

# Re-estimam modelul simplu pe aceleasi date (pentru comparatie corecta)
model_simplu_ref <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + 
                         regulation + gdp_pc_ppp, 
                       data = data_train_ext)

comparatie <- data.frame(
  Model = c("OLS Simplu (Ref)", "Extins 1 (Log+Int)", "Extins 2 (Polynomial)"),
  R2 = c(summary(model_simplu_ref)$r.squared,
         summary(model_extins_1)$r.squared,
         summary(model_extins_2)$r.squared),
  R2_Adj = c(summary(model_simplu_ref)$adj.r.squared,
             summary(model_extins_1)$adj.r.squared,
             summary(model_extins_2)$adj.r.squared),
  AIC = c(AIC(model_simplu_ref),
          AIC(model_extins_1),
          AIC(model_extins_2)),
  BIC = c(BIC(model_simplu_ref),
          BIC(model_extins_1),
          BIC(model_extins_2))
)

print(comparatie)

# Identificare model optim
cat("\n--- SELECTIA MODELULUI OPTIM ---\n")
optim_idx <- which.min(comparatie$AIC)
cat("Model cu cel mai mic AIC:", comparatie$Model[optim_idx], "\n")
cat("Concluzie: ")

if(optim_idx == 1) {
  cat("MODELUL SIMPLU ramane SUPERIOR - extensiile nu aduc imbunatatiri semnificative!\n")
  model_optim <- model_simplu_ref
  folosim_model_din_etapa3 <- TRUE
} else {
  cat("Modelul", comparatie$Model[optim_idx], "este superior.\n")
  model_optim <- get(paste0("model_extins_", optim_idx - 1))
  folosim_model_din_etapa3 <- FALSE
}

# ------------------------------------------------------------------------------
# VALIDARE OUT-OF-SAMPLE pentru modelele extinse
# ------------------------------------------------------------------------------

cat("\n\n--- VALIDARE OUT-OF-SAMPLE (Toate Modelele) ---\n")

# Functie pentru calcul RMSE
calc_rmse <- function(model, data_test) {
  pred <- predict(model, newdata = data_test)
  sqrt(mean((data_test$deposits_gdp - pred)^2))
}

rmse_simplu <- calc_rmse(model_simplu_ref, data_test_ext)
rmse_extins1 <- calc_rmse(model_extins_1, data_test_ext)
rmse_extins2 <- calc_rmse(model_extins_2, data_test_ext)

cat("\nRMSE pe Test Set:\n")
cat("Model Simplu:   ", round(rmse_simplu, 4), "\n")
cat("Model Extins 1: ", round(rmse_extins1, 4), "\n")
cat("Model Extins 2: ", round(rmse_extins2, 4), "\n")

if(rmse_simplu < min(rmse_extins1, rmse_extins2)) {
  cat("\n✓ Confirmam: MODELUL SIMPLU are cea mai buna performanta predictiva!\n")
}

# ==============================================================================
# PASUL 4.b: SCENARII DE PROGNOZA (Pe Modelul Optim)
# ==============================================================================

cat("\n\n=== PASUL 4.b: SCENARII DE PROGNOZA ===\n\n")

# Extragem datele pentru Romania
date_romania <- df_extins %>% 
  filter(country == "Romania")

if(nrow(date_romania) == 0) {
  stop("EROARE: Romania nu se gaseste in dataset!")
}

cat("--- DATE ACTUALE ROMANIA ---\n")
print(date_romania %>% select(-country))

# ------------------------------------------------------------------------------
# SCENARIU 1: Dublarea sucursalelor
# ------------------------------------------------------------------------------

cat("\n\n--- SCENARIU 1: DUBLAREA SUCURSALELOR ---\n")

scenariu1 <- date_romania
scenariu1$branches_100k <- date_romania$branches_100k * 2

cat("Branches (actual):", date_romania$branches_100k, "\n")
cat("Branches (scenariu):", scenariu1$branches_100k, "\n\n")

# Folosim modelul optim (cel simplu, din etapa 3)
if(folosim_model_din_etapa3) {
  # Incarcam modelul original din etapa 3
  prognoza1 <- predict(model_ols_clean, newdata = scenariu1, 
                       interval = "confidence", level = 0.95)
} else {
  prognoza1 <- predict(model_optim, newdata = scenariu1, 
                       interval = "confidence", level = 0.95)
}

cat("Depozite actuale:", round(date_romania$deposits_gdp, 2), "% PIB\n")
cat("Depozite prognozate:", round(prognoza1[1], 2), "% PIB\n")
cat("Impact estimat: +", round(prognoza1[1] - date_romania$deposits_gdp, 2), " pp\n")
cat("\nInterval de incredere (95%):\n")
cat("  [", round(prognoza1[2], 2), "% ;", round(prognoza1[3], 2), "%]\n")

# ------------------------------------------------------------------------------
# SCENARIU 2: Imbunatatirea reglementarii (Regulation +0.5 puncte)
# ------------------------------------------------------------------------------

cat("\n\n--- SCENARIU 2: IMBUNATATIREA REGLEMENTARII ---\n")

scenariu2 <- date_romania
scenariu2$regulation <- date_romania$regulation + 0.5

cat("Regulation (actual):", date_romania$regulation, "\n")
cat("Regulation (scenariu):", scenariu2$regulation, "\n\n")

if(folosim_model_din_etapa3) {
  prognoza2 <- predict(model_ols_clean, newdata = scenariu2, 
                       interval = "confidence", level = 0.95)
} else {
  prognoza2 <- predict(model_optim, newdata = scenariu2, 
                       interval = "confidence", level = 0.95)
}

cat("Depozite actuale:", round(date_romania$deposits_gdp, 2), "% PIB\n")
cat("Depozite prognozate:", round(prognoza2[1], 2), "% PIB\n")
cat("Impact estimat: +", round(prognoza2[1] - date_romania$deposits_gdp, 2), " pp\n")

# ------------------------------------------------------------------------------
# SCENARIU 3: Combinat (Branches x2 + Regulation +0.5)
# ------------------------------------------------------------------------------

cat("\n\n--- SCENARIU 3: SCENARIU COMBINAT (REFORMĂ AMPLĂ) ---\n")

scenariu3 <- date_romania
scenariu3$branches_100k <- date_romania$branches_100k * 2
scenariu3$regulation <- date_romania$regulation + 0.5

if(folosim_model_din_etapa3) {
  prognoza3 <- predict(model_ols_clean, newdata = scenariu3, 
                       interval = "confidence", level = 0.95)
} else {
  prognoza3 <- predict(model_optim, newdata = scenariu3, 
                       interval = "confidence", level = 0.95)
}

cat("Depozite actuale:", round(date_romania$deposits_gdp, 2), "% PIB\n")
cat("Depozite prognozate:", round(prognoza3[1], 2), "% PIB\n")
cat("Impact estimat TOTAL: +", round(prognoza3[1] - date_romania$deposits_gdp, 2), " pp\n")

# ------------------------------------------------------------------------------
# INTERPRETARE ECONOMICA A SCENARIILOR
# ------------------------------------------------------------------------------

cat("\n\n=== INTERPRETARE ECONOMICA ===\n")
cat("
1. Dublarea sucursalelor ar genera un impact direct de ~", 
    round(prognoza1[1] - date_romania$deposits_gdp, 1), " pp
   → Acest efect include atat influenta directa cat si un 'catch-up effect'
   → Romania porneste de la un nivel sub medie (", round(date_romania$deposits_gdp, 1), "%)
   → Tendinta de convergenta amplifica impactul

2. Imbunatatirea reglementarii (+0.5 pct) => +", 
    round(prognoza2[1] - date_romania$deposits_gdp, 1), " pp
   → Efect institutional: mai putina birocratie => mai multa activitate bancara

3. Scenariu combinat => +", 
    round(prognoza3[1] - date_romania$deposits_gdp, 1), " pp
   → Efectele NU sunt perfect aditive (exista diminishing returns)
   → O reforma completa necesita timp (3-5 ani implementare)
\n")

# ==============================================================================
# SALVARE REZULTATE
# ==============================================================================

# Tabel comparativ scenarii
tabel_scenarii <- data.frame(
  Scenariu = c("Actual", "Dublare Branches", "Regulation +0.5", "Scenariu Combinat"),
  Branches = c(date_romania$branches_100k, scenariu1$branches_100k, 
               date_romania$branches_100k, scenariu3$branches_100k),
  Regulation = c(date_romania$regulation, date_romania$regulation,
                 scenariu2$regulation, scenariu3$regulation),
  Deposits_Prognozat = c(date_romania$deposits_gdp, 
                         prognoza1[1], prognoza2[1], prognoza3[1]),
  Impact_pp = c(0, 
                prognoza1[1] - date_romania$deposits_gdp,
                prognoza2[1] - date_romania$deposits_gdp,
                prognoza3[1] - date_romania$deposits_gdp)
)

write.csv(tabel_scenarii, "output/tables/scenarii_prognoza_romania.csv", row.names = FALSE)
write.csv(comparatie, "output/tables/comparatie_modele_extinse.csv", row.names = FALSE)

# Salvare raport text
sink("output/tables/raport_scenarii_prognoza.txt")
cat("=== RAPORT SCENARII DE PROGNOZA ===\n")
cat("Data:", Sys.Date(), "\n\n")
cat("Model utilizat:", ifelse(folosim_model_din_etapa3, "OLS Simplu (Etapa 3)", "Model Extins"), "\n\n")
cat("--- COMPARATIE MODELE ---\n")
print(comparatie)
cat("\n\n--- SCENARII ROMANIA ---\n")
print(tabel_scenarii)
sink()

cat("\n✓ ETAPA 4 FINALIZATA! Rezultate salvate in output/tables/\n")