# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: ANALIZA AVANSATA (EXPLORATORIE 2.0)
#
# SCRIPT: a1_02.f_analiza_avansata_cluster_pca.R
# RESPONSABIL: Student 1 (Analist) / Student 3 (Data Scientist)
#
# OBIECTIVE:
# 1. Heatmap de corelatii pentru variabile transformate (log).
# 2. PCA (Analiza Componentelor Principale) - harta tarilor.
# 3. Clusterizare ierarhica a tarilor.
# 4. Identificarea "reprezentantului" (medoid) pentru fiecare cluster de tari.
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("factoextra")) install.packages("factoextra")
if (!require("corrplot")) install.packages("corrplot")
if (!require("cluster")) install.packages("cluster")

library(tidyverse)
library(factoextra)
library(corrplot)
library(cluster)

# Output
if (!dir.exists("output/advanced_analysis"))
  dir.create("output/advanced_analysis", recursive = TRUE)

# 2. Pregatirea Datelor -------------------------------------------------------
# Folosim setul complet (Train + Test) pentru pozitia tuturor tarilor
df <- read.csv("data/processed/full_data_raw.csv")

# Transformari identice cu modelul logaritmat (a1_04_model_logaritmat)
df_prep <- df %>%
  mutate(
    l_Y  = log(Y),
    l_X1 = log(X1),
    l_X2 = log(X2),
    l_X5 = log(X5)
  ) %>%
  select(l_Y, l_X1, l_X2, X3, X4, l_X5)

# Numele tarilor ca rownames
rownames(df_prep) <- df$iso_code  # sau df$country

# STANDARDIZARE (obligatoriu pentru PCA / HCA)
df_scaled <- scale(df_prep)

# 3. Heatmap de Corelatii -----------------------------------------------------
cor_matrix <- cor(df_prep, use = "complete.obs")

png("output/advanced_analysis/a1_02f_heatmap_corelatii.png",
    width = 800, height = 800)
corrplot(cor_matrix,
         method = "color",
         type   = "upper",
         order  = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Heatmap Corelatii (Variabile Transformate)",
         mar   = c(0,0,2,0))
dev.off()

# 4. PCA ----------------------------------------------------------------------
pca_result <- prcomp(df_scaled, center = TRUE, scale. = FALSE) # deja scalat

# Scree Plot
p_scree <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60),
                    main = "Scree Plot - Varianta explicata")
ggsave("output/advanced_analysis/a1_02f_pca_scree.png",
       plot = p_scree, width = 6, height = 4)

# Biplot tari + variabile
p_biplot <- fviz_pca_biplot(
  pca_result,
  repel   = TRUE,
  col.var = "#2E9FDF",
  col.ind = "#696969",
  title   = "PCA Biplot - Tari & Variabile"
)
ggsave("output/advanced_analysis/a1_02f_pca_biplot.png",
       plot = p_biplot, width = 10, height = 8)

# 5. Clusterizare Ierarhica pe tari ------------------------------------------
dist_mat   <- dist(df_scaled, method = "euclidean")
hca_result <- hclust(dist_mat, method = "ward.D2")

k_groups <- 4

png("output/advanced_analysis/a1_02f_dendrograma_tari.png",
    width = 1000, height = 600)
fviz_dend(hca_result, k = k_groups,
          cex      = 0.6,
          k_colors = "jco",
          rect     = TRUE,
          main     = "Dendrograma Tarilor (Clusterizare ierarhica)")
dev.off()

# 6. Reprezentanti (medoizi) pe clustere de tari -----------------------------
sub_grp <- cutree(hca_result, k = k_groups)

find_representative <- function(df_scale, cluster_assignments) {
  df_w_cluster <- as.data.frame(df_scale)
  df_w_cluster$Cluster <- cluster_assignments
  df_w_cluster$Country <- rownames(df_scale)
  
  reps <- data.frame()
  for (c in sort(unique(cluster_assignments))) {
    cluster_data <- df_w_cluster %>% filter(Cluster == c)
    center <- colMeans(cluster_data %>% select(-Cluster, -Country))
    distances <- apply(cluster_data %>% select(-Cluster, -Country), 1,
                       function(x) sqrt(sum((x - center)^2)))
    min_idx <- which.min(distances)
    reps <- rbind(reps,
                  data.frame(
                    Cluster      = c,
                    Reprezentant = cluster_data$Country[min_idx],
                    Distanta     = distances[min_idx]
                  ))
  }
  reps[order(reps$Cluster), ]
}

reprezentanti <- find_representative(df_scaled, sub_grp)
print(reprezentanti)

write.csv(reprezentanti,
          "output/advanced_analysis/a1_02f_reprezentanti_tari.csv",
          row.names = FALSE)

# 7. Profilul clusterelor -----------------------------------------------------
df_final_cluster <- df %>% mutate(Cluster = factor(sub_grp))

profil <- df_final_cluster %>%
  group_by(Cluster) %>%
  summarise(
    Nr_Tari      = n(),
    Avg_Depozite = mean(Y,  na.rm = TRUE),
    Avg_PIB      = mean(X5, na.rm = TRUE),
    Avg_Digital  = mean(X2, na.rm = TRUE),
    Avg_Legal    = mean(X3, na.rm = TRUE)
  ) %>%
  arrange(Cluster)

print(profil)
write.csv(profil,
          "output/advanced_analysis/a1_02f_profil_clustere.csv",
          row.names = FALSE)

# Salvarea setului complet de date, inclusiv coloana 'Cluster'
write.csv(df_final_cluster, "data/processed/full_data_clusters.csv", row.names = FALSE)

# ==============================================================================
# 8. Generarea seturilor TRAIN si TEST cu Clustere (CORECAT)
# ==============================================================================

# Citim ID-urile originale pentru a sti care tari sunt in Train si care in Test
if(file.exists("data/processed/train_data_raw.csv") & file.exists("data/processed/test_data_raw.csv")) {
  
  train_ids <- read.csv("data/processed/train_data_raw.csv") %>% pull(iso_code)
  test_ids  <- read.csv("data/processed/test_data_raw.csv") %>% pull(iso_code)
  
  # Setul Train cu Clustere
  # ATENTIE: df_final_cluster are deja iso_code corect din citirea initiala
  train_data_clusters <- df_final_cluster %>% 
    filter(iso_code %in% train_ids) %>%
    select(iso_code, country, Y, X1, X2, X3, X4, X5, Cluster)
  
  # Setul Test cu Clustere
  test_data_clusters <- df_final_cluster %>% 
    filter(iso_code %in% test_ids) %>%
    select(iso_code, country, Y, X1, X2, X3, X4, X5, Cluster)
  
  # Salvare
  write.csv(train_data_clusters, "data/processed/train_data_clusters.csv", row.names = FALSE)
  write.csv(test_data_clusters,  "data/processed/test_data_clusters.csv", row.names = FALSE)
} else {
  warning("Nu am gasit fisierele raw (train/test) pentru a face impartirea.")
}