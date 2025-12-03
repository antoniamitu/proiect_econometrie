# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: DIAGNOSTIC OUTLIERS (VALORI ABERANTE)
#
# SCRIPT: a1_02.5_diagnostic_outliers.R
# RESPONSABIL: Student 2 (Econometricianul) / Student 1 (Analist)
#
# METODOLOGIE:
# 1. Identificare vizuala prin Boxplot
# 2. Identificare statistica prin Distanta Cook
# 3. Crearea unui subset de date fara outlieri pentru comparatie 
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("olsrr")) install.packages("olsrr")
if (!require("stargazer")) install.packages("stargazer")
if (!require("ggrepel")) install.packages("ggrepel")

library(tidyverse)
library(olsrr)
library(stargazer)
library(ggrepel)

# Foldere output --------------------------------------------------------------
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tests")) dir.create("output/tests", recursive = TRUE)

# 2. Importarea Datelor de Antrenare (RAW) ------------------------------------
df <- read.csv("data/processed/train_data_raw.csv")

# Asiguram tipurile corecte
df$D <- as.factor(df$D)

# 3. Identificare Vizuala (Boxplots) -----------------------------------------
# Functie pentru a gasi outliers intr-un vector (regula 1.5 * IQR)
is_outlier <- function(x) {
  x < quantile(x, 0.25) - 1.5 * IQR(x) |
    x > quantile(x, 0.75) + 1.5 * IQR(x)
}

# Adaugam un flag in date pentru outliers pe Y
df_plot <- df %>%
  mutate(is_outlier_Y = is_outlier(Y))

# Grafic Boxplot Y cu etichete pentru tari (coloana corecta: country)
p_box <- ggplot(df_plot, aes(x = "", y = Y)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  geom_text_repel(
    data = subset(df_plot, is_outlier_Y),
    aes(label = country),
    box.padding = 0.5
  ) +
  labs(
    title = "Boxplot Y (Depozite % PIB) - Identificare Outliers",
    y = "Deposits % GDP",
    x = ""
  ) +
  theme_minimal()

# Afisam si salvam boxplot-ul
print(p_box)
ggsave("output/figures/a1_02_5_boxplot_Y_outliers.png",
       plot = p_box, width = 6, height = 4, dpi = 300)

# 4. Identificare Statistica (Distanta Cook) ---------------------------------
# Model OLS initial pentru a calcula distanta Cook
model_ols_raw <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + D, data = df)

# Graficul Distantei Cook - salvat in figures
png("output/figures/a1_02_5_cooks_distance.png",
    width = 800, height = 600)
ols_plot_cooksd_bar(model_ols_raw)
dev.off()

# Extragem valorile Cook's Distance
cooks_d <- cooks.distance(model_ols_raw)

# Pragul de taiere: 4/n
n <- nrow(df)
threshold <- 4 / n

# Identificam observatiile care depasesc pragul
outliers_indices <- which(cooks_d > threshold)
outliers_data <- df[outliers_indices, ]

cat("\n=======================================================\n")
cat(" OBSERVATII IDENTIFICATE CA OUTLIERS (Cook > 4/n) \n")
cat("=======================================================\n")
print(outliers_data %>% select(iso_code, country, Y, X5, D))

# Salvam tabelul cu outlieri in output/tests
write.csv(outliers_data,
          "output/tests/a1_02_5_outliers_cooks_distance.csv",
          row.names = FALSE)

# 5. Tratarea Outlierilor (Crearea Dataset-ului 'Clean') ---------------------
df_no_outliers <- df[-outliers_indices, ]

cat("\nNumar observatii initiale:", nrow(df))
cat("\nNumar observatii dupa eliminare outliers:", nrow(df_no_outliers), "\n")

# Salvam acest dataset pentru a-l testa in a1_03 (Regresie)
write.csv(df_no_outliers,
          "data/processed/train_data_no_outliers.csv",
          row.names = FALSE)

# Optional: sumar comparativ cu/ fara outlieri
sink("output/tests/a1_02_5_summary_with_without_outliers.txt")
cat("=== Sumar cu outlieri (train_data_raw) ===\n")
stargazer(df, type = "text", digits = 2)
cat("\n=== Sumar fara outlieri (train_data_no_outliers) ===\n")
stargazer(df_no_outliers, type = "text", digits = 2)
sink()

# 6. Nota pentru Raport ------------------------------------------------------
# Daca tarile outlier sunt economii foarte dezvoltate / atipice,
# argumentul poate fi SA NU le stergem, ci sa aplicam logaritmarea
# in iteratia urmatoare (Iteratia 2), deoarece log-ul comprima valorile mari.