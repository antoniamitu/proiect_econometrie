# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: REGRESIE LINIARA CLASICA (OLS) PE DATE BRUTE
#
# SCRIPT: a1_03_regresie_clasica.R
# RESPONSABIL: Student 2 (Econometricianul)
#
# OBIECTIVE:
# 1. Estimarea modelului OLS: Y ~ X1 + X2 + X3 + X4 + X5 + D
# 2. Testarea ipotezelor clasice (Gauss-Markov):
#    - Normalitatea reziduurilor (Jarque-Bera, Shapiro-Wilk)
#    - Homoscedasticitate (Breusch-Pagan, White-style)
#    - Multicoliniaritate (VIF)
#    - Forma functionala (RESET test)
# 3. Demonstrarea necesitatii transformarii datelor (Logaritmare)
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stargazer")) install.packages("stargazer")
if (!require("lmtest")) install.packages("lmtest")      # BP test, RESET test
if (!require("tseries")) install.packages("tseries")    # Jarque-Bera
if (!require("car")) install.packages("car")            # VIF
if (!require("sandwich")) install.packages("sandwich")  # Robust SE (optional)

library(tidyverse)
library(stargazer)
library(lmtest)
library(tseries)
library(car)
library(sandwich)

# Creare foldere output daca nu exista
if (!dir.exists("output/tests")) dir.create("output/tests", recursive = TRUE)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# 2. Importarea Datelor de Antrenare (RAW) ------------------------------------
# Lucram pe setul de antrenare CU outlieri (baseline „brut”)
df <- read.csv("data/processed/train_data_raw.csv")
df$D <- as.factor(df$D) # Asiguram ca Dummy e factor

# 3. Estimarea Modelului OLS (Level-Level) -----------------------------------
# Y = beta0 + beta1*X1 + ... + u
model_raw <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + D, data = df)

# Rezumatul modelului in consola
summary(model_raw)

# Salvarea tabelului de regresie (pentru Raport)
stargazer(model_raw, type = "text",
          title = "Rezultate Regresie OLS - Date Brute",
          out   = "output/tests/a1_03_rezultate_regresie_raw.txt")

# 4. Diagnosticarea Ipotezelor (De ce modelul brut NU e bun?) ----------------

resid_raw <- residuals(model_raw)
fitted_raw <- fitted(model_raw)

## A. Normalitatea Reziduurilor -----------------------------------------------
# H0: Reziduurile sunt distribuite normal

# Jarque-Bera (recomandat pentru n > 50)
jb_test <- jarque.bera.test(resid_raw)

# Shapiro-Wilk (mai sensibil, dar clasic)
sw_test <- shapiro.test(resid_raw)

# Histograma Reziduurilor + curba normala
png("output/figures/a1_03_hist_residuals_raw.png", width = 800, height = 600)
hinfo <- hist(resid_raw, breaks = 20, col = "lightblue",
              main = "Histograma Reziduurilor (Raw)",
              xlab = "Reziduuri")
curve(dnorm(x, mean = mean(resid_raw), sd = sd(resid_raw)) *
        length(resid_raw) * diff(hinfo$breaks)[1],
      add = TRUE, col = "red", lwd = 2)
dev.off()

## B. Homoscedasticitatea -----------------------------------------------------
# H0: Erorile sunt homoscedastice (Var(u|X) constanta)

# Testul Breusch-Pagan clasic
bp_test <- bptest(model_raw)

# „White-style” folosind bptest cu fitted + fitted^2 (vezi curs de heteroscedasticitate)
bp_white <- bptest(model_raw, ~ fitted_raw + I(fitted_raw^2))

# Grafic Residuals vs Fitted
png("output/figures/a1_03_residuals_vs_fitted_raw.png", width = 800, height = 600)
plot(model_raw, which = 1)  # plot 1 = Residuals vs Fitted
dev.off()

## C. Multicoliniaritate ------------------------------------------------------
# VIF > 10 indica problema grava
vif_vals <- vif(model_raw)

## D. Forma functionala (RESET test) -----------------------------------------
# H0: model corect specificat (liniar in parametri, fara termeni neomisi)
reset_test <- resettest(model_raw, power = 2:3, type = "fitted")

# 5. Salvarea rezultatelor diagnosticului ------------------------------------

sink("output/tests/a1_03_diagnostic_raw.txt")
cat("=== DIAGNOSTIC MODEL DATE BRUTE ===\n\n")

cat("1. NORMALITATE REZIDUURI\n")
cat("Jarque-Bera:\n")
print(jb_test)
cat("\nShapiro-Wilk:\n")
print(sw_test)

cat("\n2. HOMOSCEDASTICITATE\n")
cat("Breusch-Pagan (clasic):\n")
print(bp_test)
cat("\nWhite-style (bptest cu fitted + fitted^2):\n")
print(bp_white)

cat("\n3. MULTICOLINIARITATE (VIF):\n")
print(vif_vals)

cat("\n4. FORMA FUNCTIONALA (RESET test):\n")
print(reset_test)
sink()

# 5. Comparatie Rapida cu Modelul Fara Outlieri (Optional) -------------------
if (file.exists("data/processed/train_data_no_outliers.csv")) {
  df_clean <- read.csv("data/processed/train_data_no_outliers.csv")
  df_clean$D <- as.factor(df_clean$D)
  
  model_clean <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + D, data = df_clean)
  
  # Comparam coeficientii (nu doar R^2)
  sink("output/tests/a1_03_comparatie_raw_vs_no_outliers.txt")
  cat("=== COMPARATIE MODELE: CU vs FARA OUTLIERI ===\n\n")
  stargazer(model_raw, model_clean, type = "text",
            column.labels = c("Cu Outlieri", "Fara Outlieri"),
            title = "Comparatie Modele OLS (Level-Level)")
  cat("\nTest Jarque-Bera model fara outlieri:\n")
  print(jarque.bera.test(residuals(model_clean)))
  sink()
}

# (Optional) 6. Eroare standard robusta --------------------------------------
# Daca BP/White resping homoscedasticitatea, poti raporta si un model cu SE robuste:
# coeftest(model_raw, vcov = vcovHC(model_raw, type = "HC1"))

# 7. CONCLUZIE PENTRU RAPORT -------------------------------------------------
# De comentat in raport pe baza fisierului a1_03_diagnostic_raw.txt:
# - p-value mici la Jarque-Bera / Shapiro => reziduurile nu sunt normale
# - p-value mic la Breusch-Pagan / White => heteroscedasticitate
# - VIF mari (ex. pentru X3, X4, X5) => multicoliniaritate
# - RESET respinge H0 => forma functionala invalida (lipsesc termeni, eventual log-uri)
#
# => Toate acestea, impreuna cu asimetria din a1_02 si prezenta outlierilor din a1_02.5,
#    motiveaza trecerea la transformari logaritmice si/sau alte specificatii
#    (log-log, semi-log, modele cu variabile transformate) in aplicatia urmatoare.
