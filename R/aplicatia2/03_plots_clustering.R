# ==============================================================================
# Descriere: Vizualizari avansate si analiza de Clustering (K-Means)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, cluster, factoextra, ggpubr, pheatmap, RColorBrewer)

# 1. Incarcare Date ------------------------------------------------------------
data_path <- here("data", "processed_2", "panel_final_2.rds")
if (!file.exists(data_path)) stop("Fisierul de date nu exista!")
df <- readRDS(data_path)

# 2. Pregatire date pentru Clustering (Agregare pe tari) -----------------------
# Calculam media indicatorilor pentru perioada 2013-2023 pentru a obtine profilul structural
df_cluster <- df %>%
  group_by(iso_code, country) %>%
  summarise(
    Y_mean = mean(Y, na.rm = TRUE),
    X1_mean = mean(X1, na.rm = TRUE),
    X2_mean = mean(X2, na.rm = TRUE),
    X3_mean = mean(X3, na.rm = TRUE),
    X4_mean = mean(X4, na.rm = TRUE),
    X5_mean = mean(X5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  column_to_rownames(var = "country") %>%
  select(-iso_code) %>%
  drop_na()

# Standardizare (Scaling) - crucial pentru K-Means
df_scaled <- scale(df_cluster)

# 3. Determinarea numarului optim de clustere ----------------------------------
p_elbow <- fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(title = "Metoda Elbow", subtitle = "Numar optim de clustere")

p_sil <- fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(title = "Metoda Silhouette", subtitle = "Calitatea partitionarii")

ggsave(here("output", "plots", "cluster_diagnostics.png"), 
       ggarrange(p_elbow, p_sil, ncol = 2), width = 10, height = 5)

# 4. Aplicare K-Means (k=3 selectat pe baza analizei economice si Elbow) -------
set.seed(123)
k <- 3
km_res <- kmeans(df_scaled, centers = k, nstart = 25)

# Vizualizare Clustere (PCA projection)
plot_pca <- fviz_cluster(km_res, data = df_scaled,
                         palette = "jco", 
                         ggtheme = theme_minimal(),
                         main = "Clustering Tari: Tipologii Bancare (2013-2023)",
                         xlab = "Dimensiune & Infrastructura (PC1)", 
                         ylab = "Cadru Institutional (PC2)")
ggsave(here("output", "plots", "cluster_map.png"), plot_pca, width = 10, height = 7)

# 5. Profilarea Clusterelor (Interpretare) -------------------------------------
df_cluster$Cluster <- as.factor(km_res$cluster)

cluster_profile <- df_cluster %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean)) %>%
  mutate(across(where(is.numeric), round, 2))

print("Profilul Clusterelor (Medii):")
print(cluster_profile)
write_csv(cluster_profile, here("output", "tables_2", "cluster_profile.csv"))

# Vizualizare Heatmap pentru densitatea datelor
pheatmap(as.matrix(df_scaled), 
         cutree_rows = k,
         main = "Heatmap Indicatori Standardizati",
         filename = here("output", "plots", "cluster_heatmap.png"),
         width = 10, height = 12)