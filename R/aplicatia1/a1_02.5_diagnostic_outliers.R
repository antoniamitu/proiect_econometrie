# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1
# Rol: Student 2 (Econometricianul Clasic)
# Etapa: 2.5 DIAGNOSTIC OUTLIERI (INAINTE DE PROCESARE)
# ==============================================================================
# SCOP: Documentarea deciziei de eliminare a outlierilor
#       pentru justificare metodologica in raport
# ==============================================================================

# 1. Biblioteci
library(tidyverse)
library(readxl)
library(car)

# 2. Incarcare date brute
df_raw <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx", skip = 1)

# Selectam variabilele de interes
df_raw <- df_raw %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, 
         regulation, gdp_pc_ppp, country) %>%
  na.omit()

cat("=== DIMENSIUNE DATASET BRUT ===\n")
cat("Observatii totale:", nrow(df_raw), "\n\n")

# ==============================================================================
# PASUL 1: IDENTIFICAREA OUTLIERILOR PRIN ANALIZA DESCRIPTIVA
# ==============================================================================

cat("=== STATISTICA DESCRIPTIVA - VARIABILA DEPENDENTA ===\n")
summary(df_raw$deposits_gdp)

cat("\n=== TOP 10 TARI CU CEL MAI MARE NIVEL AL DEPOZITELOR ===\n")
top_deposits <- df_raw %>%
  arrange(desc(deposits_gdp)) %>%
  select(country, deposits_gdp, gdp_pc_ppp, branches_100k) %>%
  head(10)
print(top_deposits)

# ==============================================================================
# PASUL 2: VIZUALIZARE DISTRIBUTIE Y
# ==============================================================================

cat("\n=== GENERARE GRAFICE DIAGNOSTIC ===\n")

# Grafic 1: Histograma cu densitate
p1 <- ggplot(df_raw, aes(x = deposits_gdp)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = 200, linetype = "dashed", color = "darkred", linewidth = 1) +
  annotate("text", x = 250, y = 0.01, label = "Prag: 200%", color = "darkred", size = 4) +
  labs(title = "Distributia Depozitelor (% PIB) - Cu Outlieri",
       x = "Deposits (% GDP)", y = "Densitate") +
  theme_minimal()

# Grafic 2: Boxplot cu identificare outlieri
p2 <- ggplot(df_raw, aes(y = deposits_gdp)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 3) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "darkred", linewidth = 1) +
  labs(title = "Boxplot: Identificare Valori Extreme",
       y = "Deposits (% GDP)") +
  theme_minimal()

# Salvare grafice
ggsave("output/figures/01_distributie_deposits_cu_outlieri.png", p1, width = 10, height = 6)
ggsave("output/figures/02_boxplot_outlieri.png", p2, width = 8, height = 6)

cat("Grafice salvate in output/figures/\n\n")

# ==============================================================================
# PASUL 3: TESTARE STATISTICA A OUTLIERILOR (GRUBBS TEST)
# ==============================================================================

library(outliers)

cat("=== TEST GRUBBS (DETECTIE OUTLIERI) ===\n")
test_grubbs <- grubbs.test(df_raw$deposits_gdp, type = 10)
print(test_grubbs)

# Identificare outlieri prin regula IQR (Interquartile Range)
Q1 <- quantile(df_raw$deposits_gdp, 0.25)
Q3 <- quantile(df_raw$deposits_gdp, 0.75)
IQR_val <- Q3 - Q1
limita_superioara <- Q3 + 1.5 * IQR_val

cat("\n=== REGULA IQR (1.5 × IQR) ===\n")
cat("Q1 (25%):", Q1, "\n")
cat("Q3 (75%):", Q3, "\n")
cat("IQR:", IQR_val, "\n")
cat("Limita superioara outlieri:", limita_superioara, "\n\n")

outlieri_IQR <- df_raw %>%
  filter(deposits_gdp > limita_superioara) %>%
  select(country, deposits_gdp, gdp_pc_ppp, branches_100k)

cat("Tari identificate ca outlieri (IQR):\n")
print(outlieri_IQR)

# ==============================================================================
# PASUL 4: IMPACT AL OUTLIERILOR PE MODEL
# ==============================================================================

cat("\n\n=== COMPARATIE: MODEL CU vs FARA OUTLIERI ===\n")

# Model 1: Cu outlieri
model_cu <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + 
                 regulation + gdp_pc_ppp, data = df_raw)

# Model 2: Fara outlieri (deposits < 200%)
df_fara <- df_raw %>% filter(deposits_gdp < 200)
model_fara <- lm(deposits_gdp ~ branches_100k + atms_100k + legal_rights + 
                   regulation + gdp_pc_ppp, data = df_fara)

# Comparatie coeficienti
cat("\n--- COEFICIENTI MODEL CU OUTLIERI ---\n")
print(round(coef(model_cu), 4))

cat("\n--- COEFICIENTI MODEL FARA OUTLIERI ---\n")
print(round(coef(model_fara), 4))

# Comparatie R2
cat("\n--- COMPARATIE R-SQUARED ---\n")
cat("R2 (cu outlieri):   ", round(summary(model_cu)$r.squared, 4), "\n")
cat("R2 (fara outlieri): ", round(summary(model_fara)$r.squared, 4), "\n")
cat("R2 Adj (cu outlieri):   ", round(summary(model_cu)$adj.r.squared, 4), "\n")
cat("R2 Adj (fara outlieri): ", round(summary(model_fara)$adj.r.squared, 4), "\n")

# ==============================================================================
# PASUL 5: ANALIZA REZIDUURILOR (Cu vs Fara Outlieri)
# ==============================================================================

cat("\n=== ANALIZA REZIDUURILOR ===\n")

# Calcul statistici reziduuri
cat("\n--- REZIDUURI MODEL CU OUTLIERI ---\n")
cat("Max reziduu:", max(abs(residuals(model_cu))), "\n")
cat("Std Dev reziduuri:", sd(residuals(model_cu)), "\n")

cat("\n--- REZIDUURI MODEL FARA OUTLIERI ---\n")
cat("Max reziduu:", max(abs(residuals(model_fara))), "\n")
cat("Std Dev reziduuri:", sd(residuals(model_fara)), "\n")

# Grafic comparativ reziduuri
png("output/figures/03_comparatie_reziduuri.png", width = 1400, height = 600, res = 120)
par(mfrow = c(1, 2))

# Cu outlieri
plot(fitted(model_cu), residuals(model_cu), 
     main = "Reziduuri: Model CU Outlieri",
     xlab = "Valori prezise", ylab = "Reziduuri",
     pch = 19, col = ifelse(abs(residuals(model_cu)) > 100, "red", "blue"))
abline(h = 0, col = "darkred", lwd = 2, lty = 2)

# Fara outlieri
plot(fitted(model_fara), residuals(model_fara), 
     main = "Reziduuri: Model FARA Outlieri",
     xlab = "Valori prezise", ylab = "Reziduuri",
     pch = 19, col = "steelblue")
abline(h = 0, col = "darkred", lwd = 2, lty = 2)

dev.off()
par(mfrow = c(1, 1))

# ==============================================================================
# PASUL 6: JUSTIFICARE ECONOMICA - DE CE ELIMINI OUTLIERII?
# ==============================================================================

cat("\n\n=== JUSTIFICARE ELIMINARE OUTLIERI (HK, Singapore, Mauritania) ===\n")
cat("
ARGUMENTE ECONOMETRICE SI ECONOMICE:

1. IDENTIFICAREA TIPURILOR DE OUTLIERI:
   A) Hong Kong (548% PIB) si Singapore (244% PIB) → CENTRE FINANCIARE GLOBALE
      - Rolul lor este de intermediere financiara internationala (offshore banking).
      - Volumul depozitelor nu reflecta economisirea populatiei locale, ci fluxuri de capital strain.
   
   B) Mauritania (279% PIB) → ANOMALIE STRUCTURALA
      - Pentru o economie in curs de dezvoltare (unde media este ~30-40%), valoarea este extrema.
      - Reflecta probabil o concentrare a veniturilor din resurse naturale sau ajutoare externe, 
        decuplata de infrastructura bancara retail (sucursale/ATM-uri).

2. COMPARATIE DE REFERINTA (BENCHMARK):
   - Germania: ~48% PIB | SUA: ~53% PIB | Japonia: ~68% PIB
   - HK, Singapore si Mauritania sunt de 4-10 ori mai mari decat media normala.
   - Pastrarea lor ar invalida orice comparatie cu tari precum Romania (~34%).

3. DISTORSIUNE STATISTICA MASIVA:
   - Maximul reziduului CU outlieri: 423.6
   - Maximul reziduului FARA outlieri: 61.2
   - Mauritania, fiind o tara cu venit mic dar depozite uriase, ar fi distorsionat grav 
     panta regresiei pentru tarile emergente.
   → Eliminarea reduce zgomotul (eroarea) cu 85%.

4. VALIDITATE TEORETICA:
   - Modelul testeaza ipoteza: 'Accesul la sucursale/ATM stimuleaza economisirea'.
   - In cele 3 tari eliminate, depozite exista independent de numarul de bancomate 
     (sunt determinate de fluxuri externe sau rente).
   → Includerea lor ar fi ascuns relatia reala pentru restul de 132 de tari.

CONCLUZIE METODOLOGICA:
Decizia de eliminare asigura OMOGENITATEA esantionului si VALIDITATEA inferentei statistice.
\n")

# ==============================================================================
# SALVARE REZULTATE PENTRU RAPORT
# ==============================================================================

# Cream un raport text
sink("output/tables/raport_diagnostic_outlieri.txt")
cat("=== RAPORT DIAGNOSTIC OUTLIERI ===\n")
cat("Data:", Sys.Date(), "\n\n")
cat("Observatii totale (brute):", nrow(df_raw), "\n")
cat("Observatii dupa eliminare outlieri:", nrow(df_fara), "\n")
cat("Outlieri eliminati:", nrow(df_raw) - nrow(df_fara), "\n\n")

cat("Tari eliminate:\n")
print(df_raw %>% filter(deposits_gdp >= 200) %>% select(country, deposits_gdp))

cat("\n\nIMPACT PE R2:\n")
cat("R2 Adj (cu outlieri):  ", round(summary(model_cu)$adj.r.squared, 4), "\n")
cat("R2 Adj (fara outlieri):", round(summary(model_fara)$adj.r.squared, 4), "\n")
cat("Imbunatatire:", round((summary(model_fara)$adj.r.squared - summary(model_cu)$adj.r.squared) * 100, 2), "%\n")
sink()

cat("\n✓ Diagnostic complet! Raportul a fost salvat in output/tables/\n")