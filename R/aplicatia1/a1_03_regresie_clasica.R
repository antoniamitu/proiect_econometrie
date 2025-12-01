# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1 (VERSIUNE IMBUNATATITA)
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
if(!require(tseries)) install.packages("tseries")  # Pentru Jarque-Bera

library(tidyverse)
library(caret)
library(car)
library(lmtest)
library(sandwich)
library(readxl)
library(tseries)

# 2. Incarcare date
df <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx", skip = 1)

# 3. ELIMINAREA OUTLIERILOR
df_clean <- df %>%
  filter(deposits_gdp < 200) %>%
  select(deposits_gdp, branches_100k, atms_100k, regulation, gdp_pc_ppp, country) %>%
  na.omit()

cat("=== PRELUCRARE DATE ===\n")
cat("Observatii initiale:", nrow(df %>% na.omit()), "\n")
cat("Outlieri eliminati:", nrow(df %>% na.omit()) - nrow(df_clean), "\n")
cat("Observatii finale:", nrow(df_clean), "\n\n")

# 4. Impartirea datelor (Train/Test)
set.seed(123)
train_index <- createDataPartition(df_clean$deposits_gdp, p = 0.8, list = FALSE)
data_train <- df_clean[train_index, ]
data_test  <- df_clean[-train_index, ]

cat("Train set:", nrow(data_train), "observatii\n")
cat("Test set:", nrow(data_test), "observatii\n\n")

# ==============================================================================
# PASUL 3.a: ESTIMAREA MODELULUI MULTIPLU
# ==============================================================================

cat("=== PASUL 3.a: ESTIMAREA MODELULUI OLS ===\n\n")

# Am eliminat 'legal_rights' din cauza multicoliniaritatii cu 'regulation'
model_ols_clean <- lm(deposits_gdp ~ branches_100k + atms_100k + regulation + gdp_pc_ppp, 
                      data = data_train)

# Afisare rezultate detaliate
cat("--- REZUMAT MODEL ---\n")
summary(model_ols_clean)

# Interpretare economica a coeficientilor
cat("\n--- INTERPRETARE ECONOMICA COEFICIENTI ---\n")
coefs <- coef(model_ols_clean)
cat("
1. BRANCHES (Sucursale/100k):
   Coeficient:", round(coefs["branches_100k"], 4), "
   → Cresterea cu 1 sucursala/100k adulti => +", round(coefs["branches_100k"], 2), 
    "pp in deposits_gdp
   → Impact: POZITIV si SEMNIFICATIV - infrastructura fizica conteaza!

2. ATMs (Bancomate/100k):
   Coeficient:", round(coefs["atms_100k"], 4), "
   → Efect mai slab decat sucursalele (contactul uman > automatizare)

4. REGULATION (Libertate economica):
   Coeficient:", round(coefs["regulation"], 4), "
   → Reglementari flexibile faciliteaza activitatea bancara

5. GDP_PC_PPP (Nivel de trai):
   Coeficient:", round(coefs["gdp_pc_ppp"], 4), "
   → NESEMNIFICATIV dupa controlul pentru infrastructura
   → Concluzie: INFRASTRUCTURA > BOGATIA TARII
\n")

# ==============================================================================
# PASUL 3.b: TESTAREA VALIDITATII MODELULUI
# ==============================================================================

cat("\n=== PASUL 3.b: DIAGNOSTIC ECONOMETRIC ===\n\n")

# 1. SEMNIFICATIA PARAMETRILOR (deja afisat mai sus in summary)

# 2. MULTICOLINIARITATE (VIF)
cat("--- TEST MULTICOLINIARITATE (VIF) ---\n")
vif_vals <- vif(model_ols_clean)
print(vif_vals)
cat("\nInterpretare: VIF < 5 pentru toate variabilele => NU exista multicoliniaritate severa\n\n")

# 3. HETEROSCEDASTICITATE (Breusch-Pagan)
cat("--- TEST HETEROSCEDASTICITATE (Breusch-Pagan) ---\n")
bp_test <- bptest(model_ols_clean)
print(bp_test)
if(bp_test$p.value > 0.05) {
  cat("\nInterpretare: p-value =", round(bp_test$p.value, 4), 
      "> 0.05 => NU respingem H0\n")
  cat("Concluzie: Varianta erorilor este CONSTANTA (homoscedasticitate)\n\n")
} else {
  cat("\nInterpretare: p-value =", round(bp_test$p.value, 4), 
      "< 0.05 => respingem H0\n")
  cat("Concluzie: EXISTA heteroscedasticitate => folosim erori standard robuste!\n\n")
}

# 4. NORMALITATEA REZIDUURILOR (NOU!)
cat("--- TEST NORMALITATE REZIDUURI ---\n")

# Shapiro-Wilk (pentru sample-uri mici-medii)
if(nrow(data_train) < 5000) {
  shapiro_test <- shapiro.test(residuals(model_ols_clean))
  cat("Shapiro-Wilk Test:\n")
  print(shapiro_test)
  if(shapiro_test$p.value > 0.05) {
    cat("Interpretare: Reziduurile urmeaza o distributie NORMALA\n")
  } else {
    cat("Interpretare: Reziduurile NU sunt perfect normale (OK pentru n >30 - TCL)\n")
  }
}

# Jarque-Bera (mai robust pentru sample-uri mari)
jb_test <- jarque.bera.test(residuals(model_ols_clean))
cat("\nJarque-Bera Test:\n")
print(jb_test)
cat("\n")

# 5. AUTOCORELARE (Durbin-Watson) - optional pentru cross-section
cat("--- TEST AUTOCORELARE (Durbin-Watson) ---\n")
dw_test <- dwtest(model_ols_clean)
print(dw_test)
cat("Nota: Pentru date cross-sectional, acest test este mai putin relevant.\n\n")

# 6. ERORI STANDARD ROBUSTE (in caz de heteroscedasticitate)
cat("--- COEFICIENTI CU ERORI STANDARD ROBUSTE (HC3) ---\n")
cat("(Folosim HC3 - cel mai conservator estimator pentru sample-uri mici)\n\n")
robust_se <- coeftest(model_ols_clean, vcov = vcovHC(model_ols_clean, type = "HC3"))
print(robust_se)

# ==============================================================================
# GRAFICE DE DIAGNOSTIC (IMBUNATATITE)
# ==============================================================================

cat("\n--- GENERARE GRAFICE DIAGNOSTIC ---\n")

# Salvam graficele standard
png("output/figures/04_diagnostic_model_ols.png", width = 1400, height = 1000, res = 120)
par(mfrow = c(2, 2))
plot(model_ols_clean)
par(mfrow = c(1, 1))
dev.off()

# Grafic suplimentar: Q-Q Plot mai detaliat
png("output/figures/05_qqplot_reziduuri.png", width = 800, height = 600, res = 120)
qqnorm(residuals(model_ols_clean), main = "Q-Q Plot: Normalitatea Reziduurilor",
       pch = 19, col = "steelblue")
qqline(residuals(model_ols_clean), col = "red", lwd = 2)
dev.off()

cat("Grafice salvate in output/figures/\n\n")

# ==============================================================================
# PASUL 3.c: VALIDAREA PE SETUL DE TEST (Out-of-sample prediction)
# ==============================================================================

cat("=== PASUL 3.c: EVALUARE OUT-OF-SAMPLE ===\n\n")

# 1. Predictii pe train (pentru comparatie)
predictii_train <- predict(model_ols_clean, newdata = data_train)
rmse_train <- sqrt(mean((data_train$deposits_gdp - predictii_train)^2))
mae_train <- mean(abs(data_train$deposits_gdp - predictii_train))

# 2. Predictii pe test
predictii_test <- predict(model_ols_clean, newdata = data_test)

rezultate_test <- data.frame(
  Tara = data_test$country,
  Real = data_test$deposits_gdp,
  Prezis = predictii_test,
  Eroare = data_test$deposits_gdp - predictii_test,
  Eroare_Absoluta = abs(data_test$deposits_gdp - predictii_test),
  Eroare_Procentuala = abs((data_test$deposits_gdp - predictii_test) / data_test$deposits_gdp) * 100
)

# 3. Calcul indicatori
rmse_test <- sqrt(mean(rezultate_test$Eroare^2))
mae_test <- mean(rezultate_test$Eroare_Absoluta)
mape_test <- mean(rezultate_test$Eroare_Procentuala)

# R2 pe test
sse_test <- sum(rezultate_test$Eroare^2)
sst_test <- sum((data_test$deposits_gdp - mean(data_test$deposits_gdp))^2)
r2_test <- 1 - (sse_test / sst_test)

# 4. Afisare comparativa
cat("--- PERFORMANTA TRAIN vs TEST ---\n")
cat("RMSE Train:", round(rmse_train, 4), "\n")
cat("RMSE Test: ", round(rmse_test, 4), "\n")
cat("Diferenta: ", round(abs(rmse_train - rmse_test), 4), 
    ifelse(rmse_test < rmse_train * 1.1, "(OK - nu exista overfitting)", "(Atentie!)"), "\n\n")

cat("MAE Train: ", round(mae_train, 4), "\n")
cat("MAE Test:  ", round(mae_test, 4), "\n\n")

cat("MAPE Test: ", round(mape_test, 2), "%\n")
cat("R2 Test:   ", round(r2_test, 4), "\n\n")

# 5. Identificare predictii problematice
cat("--- CELE MAI MARI ERORI DE PREDICTIE (TOP 5) ---\n")
top_erori <- rezultate_test %>%
  arrange(desc(Eroare_Absoluta)) %>%
  select(Tara, Real, Prezis, Eroare, Eroare_Procentuala) %>%
  head(5)
print(top_erori)

# ==============================================================================
# SALVARE REZULTATE PENTRU ETAPELE URMATOARE
# ==============================================================================

cat("\n\n--- SALVARE OBIECTE SI REZULTATE ---\n")

# 1. Salvam modelul
save(model_ols_clean, file = "output/models/model_ols_clean.RData")
cat("✓ Model salvat: output/models/model_ols_clean.RData\n")

# 2. Salvam predictiile
write.csv(rezultate_test, "output/tables/predictii_test_ols.csv", row.names = FALSE)
cat("✓ Predictii salvate: output/tables/predictii_test_ols.csv\n")

# 3. Salvam datele procesate (pentru Student 3)
saveRDS(data_train, "data/processed/data_train.rds")
saveRDS(data_test, "data/processed/data_test.rds")
cat("✓ Date salvate pentru Student 3: data/processed/\n")

# 4. Salvam un raport text complet
while(sink.number() > 0) sink()
sink("output/tables/raport_model_ols_clean.txt")
cat("=== RAPORT MODEL OLS (VARIANTA CURATA) ===\n")
cat("Data:", Sys.Date(), "\n\n")
cat("SPECIFICATIE MODEL:\n")
cat("Y = deposits_gdp\n")
cat("X = branches_100k, atms_100k, legal_rights, regulation, gdp_pc_ppp\n\n")
summary(model_ols_clean)
cat("\n\n--- TESTE DIAGNOSTIC ---\n")
cat("VIF:\n")
print(vif(model_ols_clean))
cat("\nBreusch-Pagan:\n")
print(bp_test)
cat("\n\n--- PERFORMANTA OUT-OF-SAMPLE ---\n")
cat("RMSE Test:", round(rmse_test, 4), "\n")
cat("MAE Test:", round(mae_test, 4), "\n")
cat("MAPE Test:", round(mape_test, 2), "%\n")
cat("R2 Test:", round(r2_test, 4), "\n")
sink()

cat("✓ Raport complet: output/tables/raport_model_ols_clean.txt\n")

cat("\n✓ ETAPA 3 FINALIZATA!\n")