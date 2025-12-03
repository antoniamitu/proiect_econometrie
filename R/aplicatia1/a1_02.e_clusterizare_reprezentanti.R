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

# Ne asiguram ca Y si X-urile sunt numerice
df$Y  <- as.numeric(df$Y)
df$X1 <- as.numeric(df$X1)
df$X2 <- as.numeric(df$X2)
df$X3 <- as.numeric(df$X3)
df$X4 <- as.numeric(df$X4)
df$X5 <- as.numeric(df$X5)

# Selectam doar variabilele X numerice pentru corelatie
vars_X_df <- df %>% select(X1, X2, X3, X4, X5)
# alternativ, daca ai deja vars_X <- df %>% select(X1:X5), poti scrie:
# vars_X_df <- vars_X

# Calculam corelatia dintre Y si fiecare X (matrice 1 x k)
cor_matrix_YX <- cor(df$Y, vars_X_df, use = "complete.obs")

# Convertim matricea intr-un vector numit (X1, X2, ...)
cor_vector <- as.numeric(cor_matrix_YX)
names(cor_vector) <- colnames(cor_matrix_YX)

# Tabel final cu info despre clustere
info_clusters <- tibble(
  Variabila = names(clusters),
  Cluster   = as.integer(clusters),
  Corr_Y    = cor_vector[names(clusters)]
)

# Reprezentantul = variabila cu |corr(Y,X)| maxim in fiecare cluster
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
