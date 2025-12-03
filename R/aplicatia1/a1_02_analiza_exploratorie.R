# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: ANALIZA EXPLORATORIE (EDA) PE DATELE BRUTE
#
# SCRIPT: a1_02_analiza_exploratorie.R
# RESPONSABIL: Student 1 (Teoreticianul/Analistul)
#
# SCOP: 
# - Inspectarea distributiei variabilelor (Y si X-uri).
# - Identificarea asimetriei (Skewness) si a boltirii (Kurtosis).
# - Vizualizarea relatiilor dintre variabile (Scatterplots).
# - Verificarea multicoliniaritatii (Matricea de corelatie).
# - JUSTIFICAREA necesitatii transformarilor (logaritmare) pentru etapa urmatoare.
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stargazer")) install.packages("stargazer")
if (!require("moments")) install.packages("moments")   # Pentru skewness/kurtosis
if (!require("corrplot")) install.packages("corrplot") # Pentru matricea de corelatie
if (!require("gridExtra")) install.packages("gridExtra") # Pentru aranjarea graficelor

library(tidyverse)
library(stargazer)
library(moments)
library(corrplot)
library(gridExtra)

# 0. Fisiere output -----------------------------------------------------------
if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tests")) dir.create("output/tests", recursive = TRUE)

# 2. Importarea Datelor de Antrenare ------------------------------------------
# Lucram DOAR pe setul de antrenare pentru a nu "spiona" setul de testare
df <- read.csv("data/processed/train_data_no_outliers.csv")

# Ne asiguram ca variabila Dummy este factor
df$D <- as.factor(df$D)

# 3. Statistici Descriptive (Tabelar) -----------------------------------------
# Salvam si in fisier text pentru raport
sink("output/tests/a1_02_descriptive_stats.txt")
stargazer(df, type = "text", 
          title = "Statistici Descriptive - Date de Antrenare (Brute)",
          digits = 2)
sink()

# Calculam explicit Skewness si Kurtosis pentru a argumenta non-normalitatea
vars_numerice <- df %>% select(Y, X1, X2, X3, X4, X5)

stats_extra <- data.frame(
  Variabila = names(vars_numerice),
  Skewness = apply(vars_numerice, 2, skewness),
  Kurtosis = apply(vars_numerice, 2, kurtosis)
)

print("Indicatori de forma a distributiei:")
print(stats_extra)

# Salvam si in CSV pentru a fi usor de citat in raport
write.csv(stats_extra,
          "output/tests/a1_02_skewness_kurtosis.csv",
          row.names = FALSE)

# INTERPRETARE PENTRU RAPORT:
# Daca Skewness > 1, avem asimetrie pozitiva puternica (coada lunga la dreapta).
# Aceasta este justificarea principala pentru logaritmare!

# 4. Analiza Grafica Univariata (Histograme) ----------------------------------

# Histograma pentru Variabila dependenta (Y - Depozite % PIB)
p1 <- ggplot(df, aes(x = Y)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distributia Y (Depozite % PIB)", x = "Y", y = "Frecventa")

# Histograma pentru X5 (PIB per capita) - de obicei foarte asimetrica
p2 <- ggplot(df, aes(x = X5)) +
  geom_histogram(bins = 20, fill = "darkred", color = "white") +
  theme_minimal() +
  labs(title = "Distributia X5 (PIB per capita)", x = "X5", y = "Frecventa")

# Afisam graficele in RStudio
grid.arrange(p1, p2, ncol = 2)

# Salvam histogramele individual
ggsave("output/figures/a1_02_hist_Y.png",  plot = p1,
       width = 6, height = 4, dpi = 300)
ggsave("output/figures/a1_02_hist_X5.png", plot = p2,
       width = 6, height = 4, dpi = 300)

# 5. Analiza Grafica Bivariata (Relatii intre variabile) ----------------------

# Y vs X5 (Relatia dintre Dezvoltare Economica si Depozite)
p3 <- ggplot(df, aes(x = X5, y = Y)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Relatia Y vs X5 (PIB)", 
       subtitle = "Observati daca punctele se departeaza de linie (Heteroscedasticitate?)")

# Y vs X1 (Sucursale)
p4 <- ggplot(df, aes(x = X1, y = Y)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Relatia Y vs X1 (Sucursale)")

# Y in functie de Dummy (High vs Low Freedom)
p5 <- ggplot(df, aes(x = D, y = Y, fill = D)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Y in functie de Libertatea Economica (Dummy)",
       x = "Categorie", y = "Y")

grid.arrange(p3, p4, p5, ncol = 2)

# Salvam si aceste grafice
ggsave("output/figures/a1_02_scatter_Y_X5.png", plot = p3,
       width = 6, height = 4, dpi = 300)
ggsave("output/figures/a1_02_scatter_Y_X1.png", plot = p4,
       width = 6, height = 4, dpi = 300)
ggsave("output/figures/a1_02_box_Y_D.png",     plot = p5,
       width = 6, height = 4, dpi = 300)

# 6. Verificarea Multicoliniaritatii (Corelatii) ------------------------------
# Matricea de corelatie doar pentru variabilele numerice
cor_matrix <- cor(vars_numerice)

# Salvam matricea numerica pentru teste
write.csv(cor_matrix,
          "output/tests/a1_02_correlation_matrix.csv",
          row.names = TRUE)

# Vizualizare cu corrplot - folosim dispozitiv PNG
png("output/figures/a1_02_corrplot.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         title = "Matricea de Corelatie", mar = c(0,0,1,0))
dev.off()

# INTERPRETARE:
# Cautam corelatii foarte mari (> 0.7 sau 0.8) intre X-uri.
# Daca exista corelatii mari, avem un argument pentru multicoliniaritate
# si pentru utilizarea tehnicilor de selectie (clustering, PCA, LASSO) in etapele urmatoare.