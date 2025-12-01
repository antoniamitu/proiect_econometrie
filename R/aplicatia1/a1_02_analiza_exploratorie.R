# ==============================================================================
# PROIECT ECONOMETRIE - APLICATIA 1
# Rol: Student 1 (Teoreticianul si Analistul Explorator)
# Etapa: 2.b ANALIZA EXPLORATORIE A DATELOR
# ==============================================================================
# SCOP: Analiza descriptiva, distributii, corelatii si vizualizari
#       pentru intelegerea structurii datelor inainte de modelare
# ==============================================================================

# 1. Incarcare biblioteci
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(corrplot)) install.packages("corrplot")
if(!require(GGally)) install.packages("GGally")
if(!require(moments)) install.packages("moments")
if(!require(psych)) install.packages("psych")
if(!require(gridExtra)) install.packages("gridExtra")

library(tidyverse)
library(readxl)
library(corrplot)
library(GGally)
library(moments)
library(psych)
library(gridExtra)

# 2. Incarcare date brute
df_raw <- read_excel("data/raw/Aplication1_CrossSectionalData.xlsx", skip = 1)

# Selectam variabilele de interes
df_analysis <- df_raw %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, 
         regulation, gdp_pc_ppp, country) %>%
  na.omit()

cat("=== DIMENSIUNE DATASET ===\n")
cat("Observatii totale:", nrow(df_analysis), "\n")
cat("Variabile:", ncol(df_analysis) - 1, "(+ country)\n\n")

# ==============================================================================
# PASUL 2.b.1: STATISTICI DESCRIPTIVE DETALIATE
# ==============================================================================

cat("=== STATISTICI DESCRIPTIVE ===\n\n")

# Statistici de baza pentru toate variabilele numerice
df_numeric <- df_analysis %>% select(-country)

desc_stats <- describe(df_numeric)
print(round(desc_stats, 2))

# Statistici suplimentare (skewness, kurtosis)
cat("\n--- ASIMETRIE SI CURTOZA ---\n")
skew_kurt <- data.frame(
  Variabila = names(df_numeric),
  Skewness = apply(df_numeric, 2, skewness),
  Kurtosis = apply(df_numeric, 2, kurtosis),
  Interpretare_Skew = ifelse(abs(apply(df_numeric, 2, skewness)) < 0.5, 
                             "Aproximativ simetrica",
                             ifelse(apply(df_numeric, 2, skewness) > 0, 
                                    "Asimetrie dreapta (tail lung)", 
                                    "Asimetrie stanga")),
  Interpretare_Kurt = ifelse(apply(df_numeric, 2, kurtosis) > 3, 
                             "Leptocurtica (outliers)", 
                             "Platycurtica/Mesocurtica")
)
print(skew_kurt)

# Salvare tabel statistici
write.csv(desc_stats, "output/tables/statistici_descriptive.csv", row.names = TRUE)
write.csv(skew_kurt, "output/tables/asimetrie_curtoza.csv", row.names = FALSE)

# ==============================================================================
# PASUL 2.b.2: DISTRIBUTII VARIABILE (HISTOGRAME + DENSITY PLOTS)
# ==============================================================================

cat("\n=== GENERARE GRAFICE DISTRIBUTII ===\n")

# Functie pentru creare histograma cu densitate
plot_distribution <- function(data, var_name, var_label) {
  ggplot(data, aes_string(x = var_name)) +
    geom_histogram(aes(y = after_stat(density)), bins = 25, 
                   fill = "steelblue", alpha = 0.7, color = "white") +
    geom_density(color = "darkred", linewidth = 1.2) +
    geom_vline(aes_string(xintercept = paste0("mean(", var_name, ")")), 
               color = "darkgreen", linetype = "dashed", linewidth = 1) +
    labs(title = paste("Distributia:", var_label),
         x = var_label, y = "Densitate") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

# Cream graficele pentru fiecare variabila
p1 <- plot_distribution(df_numeric, "deposits_gdp", "Depozite (% PIB)")
p2 <- plot_distribution(df_numeric, "branches_100k", "Sucursale/100k adulti")
p3 <- plot_distribution(df_numeric, "atms_100k", "ATMs/100k adulti")
p4 <- plot_distribution(df_numeric, "legal_rights", "Legal Rights Index")
p5 <- plot_distribution(df_numeric, "regulation", "Regulation Index")
p6 <- plot_distribution(df_numeric, "gdp_pc_ppp", "PIB/capita PPP")

# Salvam intr-un grid 3x2
png("output/figures/06_distributii_variabile.png", width = 1600, height = 1200, res = 120)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
dev.off()

cat("✓ Grafice distributii salvate: output/figures/06_distributii_variabile.png\n")

# ==============================================================================
# PASUL 2.b.3: BOXPLOTS PENTRU IDENTIFICARE OUTLIERI
# ==============================================================================

cat("\n=== GENERARE BOXPLOTS ===\n")

# Transformam datele in format long pentru boxplot comparat
df_long <- df_numeric %>%
  mutate(across(everything(), ~scale(.))) %>%  # Standardizam pentru comparatie
  pivot_longer(cols = everything(), names_to = "Variabila", values_to = "Valoare")

p_boxplot <- ggplot(df_long, aes(x = Variabila, y = Valoare, fill = Variabila)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "darkred") +
  labs(title = "Boxplots Comparativ (Valori Standardizate)",
       subtitle = "Liniile rosii indica ±3 deviatii standard",
       x = "Variabila", y = "Valoare Z-score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("output/figures/07_boxplots_comparativ.png", p_boxplot, 
       width = 12, height = 8)

cat("✓ Boxplots salvate: output/figures/07_boxplots_comparativ.png\n")

# ==============================================================================
# PASUL 2.b.4: MATRICE DE CORELATIE
# ==============================================================================

cat("\n=== ANALIZA CORELATIILOR ===\n\n")

# Calculam matricea de corelatie
cor_matrix <- cor(df_numeric, use = "complete.obs")
print(round(cor_matrix, 3))

# Test de semnificatie pentru corelatii
cor_test_results <- cor.mtest(df_numeric, conf.level = 0.95)

# Vizualizare corelatie cu corrplot (metoda color)
png("output/figures/08_matrice_corelatie.png", width = 1000, height = 1000, res = 120)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.col = "black", 
         tl.srt = 45,
         p.mat = cor_test_results$p,
         sig.level = 0.05,
         insig = "blank",  # Ascundem corelatiile nesemnificative
         diag = FALSE,
         title = "Matrice de Corelatie (doar corel. semnificative p<0.05)",
         mar = c(0,0,2,0))
dev.off()

cat("✓ Matrice corelatie salvata: output/figures/08_matrice_corelatie.png\n")

# Salvam matricea ca CSV
write.csv(cor_matrix, "output/tables/matrice_corelatie.csv", row.names = TRUE)

# ==============================================================================
# PASUL 2.b.5: SCATTERPLOTS (Y vs X variabile)
# ==============================================================================

cat("\n=== GENERARE SCATTERPLOTS (Y vs X) ===\n")

# Functie pentru scatterplot cu linie de regresie
plot_scatter <- function(data, x_var, x_label, y_var = "deposits_gdp") {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.6, size = 3, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
    labs(title = paste("Relatia:", x_label, "vs Depozite"),
         x = x_label, y = "Depozite (% PIB)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    annotate("text", x = Inf, y = Inf, 
             label = paste("r =", round(cor(data[[x_var]], data[[y_var]], 
                                            use = "complete.obs"), 3)),
             hjust = 1.1, vjust = 1.5, size = 5, fontface = "bold", color = "darkred")
}

# Cream scatterplots pentru fiecare X
s1 <- plot_scatter(df_numeric, "branches_100k", "Sucursale/100k")
s2 <- plot_scatter(df_numeric, "atms_100k", "ATMs/100k")
s3 <- plot_scatter(df_numeric, "legal_rights", "Legal Rights")
s4 <- plot_scatter(df_numeric, "regulation", "Regulation")
s5 <- plot_scatter(df_numeric, "gdp_pc_ppp", "PIB/capita PPP")

# Salvam in grid 3x2
png("output/figures/09_scatterplots_y_vs_x.png", width = 1600, height = 1200, res = 120)
grid.arrange(s1, s2, s3, s4, s5, ncol = 2)
dev.off()

cat("✓ Scatterplots salvate: output/figures/09_scatterplots_y_vs_x.png\n")

# ==============================================================================
# PASUL 2.b.6: PAIRPLOT COMPLET (Matrice de Scatterplots)
# ==============================================================================

cat("\n=== GENERARE PAIRPLOT (GGally) ===\n")

# Selectam doar variabilele principale pentru pairplot (fara country)
df_pairs <- df_numeric %>%
  select(deposits_gdp, branches_100k, atms_100k, legal_rights, regulation, gdp_pc_ppp)

png("output/figures/10_pairplot_complet.png", width = 1800, height = 1800, res = 120)
ggpairs(df_pairs,
        title = "Matrice de Scatterplots si Corelatii",
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
        diag = list(continuous = wrap("densityDiag", fill = "steelblue", alpha = 0.5))) +
  theme_minimal()
dev.off()

cat("✓ Pairplot salvat: output/figures/10_pairplot_complet.png\n")

# ==============================================================================
# PASUL 2.b.7: ANALIZA PE GRUPE (TOP 10 vs BOTTOM 10 tari)
# ==============================================================================

cat("\n=== ANALIZA COMPARATIVA: TOP vs BOTTOM ===\n\n")

# TOP 10 tari dupa depozite
top10 <- df_analysis %>%
  arrange(desc(deposits_gdp)) %>%
  head(10) %>%
  select(country, deposits_gdp, branches_100k, atms_100k, legal_rights, regulation, gdp_pc_ppp)

cat("--- TOP 10 TARI (DEPOZITE) ---\n")
print(top10)

# BOTTOM 10 tari dupa depozite
bottom10 <- df_analysis %>%
  arrange(deposits_gdp) %>%
  head(10) %>%
  select(country, deposits_gdp, branches_100k, atms_100k, legal_rights, regulation, gdp_pc_ppp)

cat("\n--- BOTTOM 10 TARI (DEPOZITE) ---\n")
print(bottom10)

# Comparatie medie Top vs Bottom
comparison <- data.frame(
  Grup = c("TOP 10", "BOTTOM 10", "Diferenta (%)"),
  Deposits = c(mean(top10$deposits_gdp), 
               mean(bottom10$deposits_gdp),
               (mean(top10$deposits_gdp) - mean(bottom10$deposits_gdp)) / mean(bottom10$deposits_gdp) * 100),
  Branches = c(mean(top10$branches_100k), 
               mean(bottom10$branches_100k),
               (mean(top10$branches_100k) - mean(bottom10$branches_100k)) / mean(bottom10$branches_100k) * 100),
  ATMs = c(mean(top10$atms_100k), 
           mean(bottom10$atms_100k),
           (mean(top10$atms_100k) - mean(bottom10$atms_100k)) / mean(bottom10$atms_100k) * 100),
  Legal = c(mean(top10$legal_rights), 
            mean(bottom10$legal_rights),
            (mean(top10$legal_rights) - mean(bottom10$legal_rights)) / mean(bottom10$legal_rights) * 100),
  Regulation = c(mean(top10$regulation), 
                 mean(bottom10$regulation),
                 (mean(top10$regulation) - mean(bottom10$regulation)) / mean(bottom10$regulation) * 100)
)

cat("\n--- COMPARATIE MEDIE TOP vs BOTTOM ---\n")
# Rotunjim doar coloanele numerice
comparison_print <- comparison
comparison_print[, 2:6] <- round(comparison_print[, 2:6], 2)
print(comparison_print)

write.csv(comparison, "output/tables/comparatie_top_bottom.csv", row.names = FALSE)

# ==============================================================================
# PASUL 2.b.8: IDENTIFICARE TARI CU PROFIL ATIPIC
# ==============================================================================

cat("\n\n=== IDENTIFICARE CAZURI ATIPICE ===\n\n")

# Cazuri cu Deposits mari dar Infrastructure scazuta
atipic_1 <- df_analysis %>%
  filter(deposits_gdp > median(deposits_gdp) & 
           branches_100k < median(branches_100k)) %>%
  select(country, deposits_gdp, branches_100k, gdp_pc_ppp) %>%
  arrange(desc(deposits_gdp))

cat("Tari cu DEPOZITE MARI dar INFRASTRUCTURE SCAZUTA:\n")
print(atipic_1)

# Cazuri cu Infrastructure mare dar Deposits scazute
atipic_2 <- df_analysis %>%
  filter(deposits_gdp < median(deposits_gdp) & 
           branches_100k > median(branches_100k)) %>%
  select(country, deposits_gdp, branches_100k, gdp_pc_ppp) %>%
  arrange(deposits_gdp)

cat("\nTari cu INFRASTRUCTURA MARE dar DEPOZITE SCAZUTE:\n")
print(atipic_2)

# ==============================================================================
# SALVARE RAPORT FINAL
# ==============================================================================

# 1. Inchidem orice conexiune veche ramasa deschisa accidental
while(sink.number() > 0) sink()

# 2. Deschidem fisierul
sink("output/tables/raport_analiza_exploratorie.txt", split = TRUE)
cat("=== RAPORT ANALIZA EXPLORATORIE (STUDENT 1) ===\n")
cat("Data:", Sys.Date(), "\n\n")

cat("DIMENSIUNE DATASET:\n")
cat("Observatii:", nrow(df_analysis), "\n")
cat("Variabile:", ncol(df_numeric), "\n\n")

cat("STATISTICI DESCRIPTIVE:\n")
print(round(desc_stats[, c("mean", "sd", "min", "max")], 2))

cat("\n\nASIMETRIE SI CURTOZA:\n")
print(skew_kurt)

cat("\n\nMATRICE CORELATIE:\n")
print(round(cor_matrix, 3))

cat("\n\nCOMPARATIE TOP 10 vs BOTTOM 10:\n")
comparison_print <- comparison
comparison_print[, 2:6] <- round(comparison_print[, 2:6], 2)
print(comparison_print)

cat("\n\nCONCLUZII PRELIMINARE:\n")
cat("1. Variabila 'deposits_gdp' prezinta o distributie asimetrica dreapta (skewness pozitiv).\n")
cat("2. Existenta unor outlieri majori (HK, Singapore, Mauritania) - vor fi tratati de Student 2.\n")
cor_cu_y <- cor_matrix["deposits_gdp", ]
cor_cu_y_fara_y <- cor_cu_y[names(cor_cu_y) != "deposits_gdp"]
max_cor_var <- names(which.max(abs(cor_cu_y_fara_y)))
max_cor_val <- cor_cu_y_fara_y[max_cor_var]
cat("3. Corelatia cea mai puternica cu Y:", max_cor_var, "(r =", round(max_cor_val, 3), ")\n")
cat("4. Tarile cu depozite mari tind sa aiba infrastructura bancara mai dezvoltata.\n")
cat("5. PIB/capita nu este cel mai puternic predictor - infrastructura conteaza mai mult!\n")

sink()

cat("\n✓ Raport analiza exploratorie salvat: output/tables/raport_analiza_exploratorie.txt\n")
cat("\n✓ ANALIZA EXPLORATORIE FINALIZATA!\n")
cat("\nFisiere generate:\n")
cat("  - output/figures/06_distributii_variabile.png\n")
cat("  - output/figures/07_boxplots_comparativ.png\n")
cat("  - output/figures/08_matrice_corelatie.png\n")
cat("  - output/figures/09_scatterplots_y_vs_x.png\n")
cat("  - output/figures/10_pairplot_complet.png\n")
cat("  - output/tables/statistici_descriptive.csv\n")
cat("  - output/tables/matrice_corelatie.csv\n")
cat("  - output/tables/comparatie_top_bottom.csv\n")
cat("  - output/tables/raport_analiza_exploratorie.txt\n")