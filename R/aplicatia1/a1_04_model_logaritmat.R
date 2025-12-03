# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: ITERATIA 2 - MODEL LOG-LOG & STANDARDIZARE
#
# SCRIPT: a1_04_model_logaritmat.R
# RESPONSABIL: Student 2 (Econometricianul)
#
# SCOP: 
# 1. Transformarea logaritmica a variabilelor (pentru a corecta asimetria).
# 2. Standardizarea predictori-lor (pentru a compara coeficientii intre ei).
# 3. Estimarea modelului LOG-LOG (si a variantei standardizate).
# 4. Validarea ipotezelor (normalitate, homoscedasticitate, forma functionala).
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stargazer")) install.packages("stargazer")
if (!require("lmtest")) install.packages("lmtest")
if (!require("tseries")) install.packages("tseries")
if (!require("car")) install.packages("car")
if (!require("sandwich")) install.packages("sandwich")

library(tidyverse)
library(stargazer)
library(lmtest)
library(tseries)
library(car)
library(sandwich)

# Output directories
if (!dir.exists("output/tests")) dir.create("output/tests", recursive = TRUE)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# 2. Importarea Datelor BRUTE (Train) -----------------------------------------
# Plecam de la datele raw; daca vrei varianta fara outlieri, schimbi path-ul.
df <- read.csv("data/processed/train_data_raw.csv")
df$D <- as.factor(df$D)

# 3. Transformarea Datelor (Logaritmare) --------------------------------------
# Logaritmam Y, X1, X2, X5 (toate strict pozitive in dataset).
# X3 si X4 sunt scoruri 1–10 => le lasam in nivel (sau standardizate mai jos).

df_log <- df %>%
  mutate(
    l_Y  = log(Y),
    l_X1 = log(X1),
    l_X2 = log(X2),
    l_X5 = log(X5)
  )

# 4. Estimarea Modelului LOG-LOG ----------------------------------------------
# Interpretare coeficienti: elasticitati (Δ%Y la 1% schimbare in X)
model_log <- lm(l_Y ~ l_X1 + l_X2 + X3 + X4 + l_X5 + D, data = df_log)

summary(model_log)

# 4.b Varianta STANDARDIZATA a predictori-lor ---------------------------------
# Standardizam DOAR X-urile continue logaritmate + scorurile:
df_log_std <- df_log %>%
  mutate(
    z_l_X1 = as.numeric(scale(l_X1)),
    z_l_X2 = as.numeric(scale(l_X2)),
    z_X3   = as.numeric(scale(X3)),
    z_X4   = as.numeric(scale(X4)),
    z_l_X5 = as.numeric(scale(l_X5))
  )

# Model cu predictori standardizati (coeficientii comparabili ca marime)
model_log_std <- lm(l_Y ~ z_l_X1 + z_l_X2 + z_X3 + z_X4 + z_l_X5 + D,
                    data = df_log_std)

# Salvarea rezultatelor (ambele modele)
stargazer(model_log, model_log_std, type = "text",
          column.labels = c("Log-Log", "Log-Log Std."),
          title = "Rezultate Regresie - Model Logaritmat (Iteratia 2)",
          out   = "output/tests/a1_04_rezultate_regresie_log.txt")

# 5. Diagnosticarea Modelului Logaritmat (ne-standardizat) --------------------
# Luam model_log ca „model structural” pe care facem teste Gauss-Markov

resid_log  <- residuals(model_log)

## A. Normalitate (Jarque-Bera)
jb_log <- jarque.bera.test(resid_log)
print(jb_log)

## B. Homoscedasticitate (Breusch-Pagan)
bp_log <- bptest(model_log)
print(bp_log)

## C. Multicoliniaritate (VIF)
vif_log <- vif(model_log)
print(vif_log)

## D. Forma functionala (RESET)
reset_log <- resettest(model_log, power = 2:3, type = "fitted")
print(reset_log)

# Salvarea diagnosticului
sink("output/tests/a1_04_diagnostic_log.txt")
cat("=== DIAGNOSTIC MODEL LOGARITMAT (ITERATIA 2) ===\n\n")
cat("1. NORMALITATE (Jarque-Bera):\n")
print(jb_log)
cat("\n2. HOMOSCEDASTICITATE (Breusch-Pagan):\n")
print(bp_log)
cat("\n3. MULTICOLINIARITATE (VIF):\n")
print(vif_log)
cat("\n4. FORMA FUNCTIONALA (RESET):\n")
print(reset_log)
sink()

# 6. Grafice de Diagnostic Final ---------------------------------------------

# Histograma Reziduurilor (cu curba normala) - corectata sa nu refaca histograma
png("output/figures/a1_04_hist_residuals_log.png", width = 800, height = 600)
hinfo <- hist(resid_log, breaks = 15, col = "lightgreen",
              main = "Histograma Reziduurilor (Model Log)",
              xlab = "Reziduuri Log")
curve(dnorm(x, mean = mean(resid_log), sd = sd(resid_log)) *
        length(resid_log) * diff(hinfo$breaks)[1],
      add = TRUE, col = "darkgreen", lwd = 2)
dev.off()

# Q-Q Plot (Punctele ar trebui sa fie mai aproape de linie decat in modelul brut)
png("output/figures/a1_04_qq_plot_log.png", width = 800, height = 600)
qqnorm(resid_log, main = "Q-Q Plot Reziduuri (Model Log)")
qqline(resid_log, col = "red", lwd = 2)
dev.off()

# Residuals vs Fitted (ar trebui sa fie mai putina „palnie”)
png("output/figures/a1_04_residuals_fitted_log.png", width = 800, height = 600)
plot(model_log, which = 1, main = "Residuals vs Fitted (Model Log)")
dev.off()

# Salvarea datelor de antrenament logaritmate pentru Studentul 3 (ML)
write.csv(df_log, "data/processed/train_data_log.csv", row.names = FALSE)