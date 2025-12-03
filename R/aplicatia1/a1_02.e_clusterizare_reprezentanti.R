# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: CLUSTERIZARE VARIABILE & ALEGERE REPREZENTANTI
#
# SCRIPT: a1_02.e_clusterizare_reprezentanti.R
# RESPONSABIL: Student 2 (Econometricianul)
#
# SCOP:
# 1. Vizualizarea corelatiilor dintre X-uri (heatmap).
# 2. Clusterizare ierarhica pe variabile explicative.
# 3. Alegerea unui reprezentant per cluster (pe baza |corr(Y, X)|).
# ==============================================================================

# 1. Pachete -------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot"))  install.packages("corrplot")

library(tidyverse)
library(corrplot)

# Fisiere output
if (!dir.exists("output/tests"))   dir.create("output/tests", recursive = TRUE)
if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# 2. Import date (fara outlieri, daca exista) ---------------------------------
# Daca vrei sa lucrezi pe datele brute, schimbi path-ul in train_data_raw.csv
path_data <- if (file.exists("data/processed/train_data_no_outliers.csv")) {
  "data/processed/train_data_no_outliers.csv"
} else {
  "data/processed/train_data_raw.csv"
}

df <- read.csv(path_data)
df$D <- as.factor(df$D)

# 3. Matrice de corelatii & heatmap -------------------------------------------
vars_X <- df %>% select(X1, X2, X3, X4, X5)
cor_X  <- cor(vars_X, use = "complete.obs")

# Salvam matricea de corelatie
write.csv(cor_X, "output/tests/a1_02e_cor_X.csv", row.names = TRUE)

# Heatmap cu corrplot
png("output/figures/a1_02e_heatmap_cor_X.png", width = 800, height = 600)
corrplot(cor_X, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Matricea de Corelatie intre X-uri", mar = c(0,0,1,0))
dev.off()

# 4. Clusterizare ierarhica pe variabile --------------------------------------
# Distanta bazata pe 1 - |correlatie| (variabile puternic corelate apropiate)
dist_mat <- as.dist(1 - abs(cor_X))
hc <- hclust(dist_mat, method = "ward.D2")

# Dendrograma
png("output/figures/a1_02e_dendrogram_X.png", width = 800, height = 600)
plot(hc, main = "Clusterizare ierarhica a variabilelor explicative",
     xlab = "", sub = "")
dev.off()

# Alegem, de exemplu, 2 clustere (se poate ajusta in functie de dendrograma)
k <- 2
clusters <- cutree(hc, k = k)

# 5. Alegerea reprezentantului pe baza corelatiei cu Y ------------------------

# Corelatia Y cu fiecare X
cor_YX <- cor(df$Y, vars_X, use = "complete.obs")

info_clusters <- tibble(
  Variabila = names(clusters),
  Cluster   = as.integer(clusters),
  Corr_Y    = as.numeric(cor_YX[names(clusters)])
)

# Pentru fiecare cluster alegem variabila cu |corr(Y,X)| maxim
reprezentanti <- info_clusters %>%
  group_by(Cluster) %>%
  slice_max(order_by = abs(Corr_Y), n = 1, with_ties = FALSE) %>%
  ungroup()

print("Informatii pe clustere:")
print(info_clusters)

print("Reprezentantii alesi pe clustere (dupa |corr(Y,X)|):")
print(reprezentanti)

# Salvam in output/tests
write.csv(info_clusters,
          "output/tests/a1_02e_info_clustere.csv", row.names = FALSE)
write.csv(reprezentanti,
          "output/tests/a1_02e_reprezentanti.csv", row.names = FALSE)

# 6. Crearea unui dataset cu reprezentantii selectati -------------------------
# Acest fisier poate fi folosit ulterior in regresii / comparatii de modele
rep_vars <- reprezentanti$Variabila

df_reps <- df %>%
  select(iso_code, country, Y, D, all_of(rep_vars))

write.csv(df_reps,
          "data/processed/train_data_cluster_reps.csv",
          row.names = FALSE)
