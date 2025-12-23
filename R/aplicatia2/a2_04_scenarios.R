# ==============================================================================
# Descriere: Simulare scenarii de politica (What-If Analysis)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, scales)

# 1. Configurare Model  ----------------------------------
# Coeficientii extrasi din modelul FE (Fixed Effects)
coefs <- list(
  X1 = 0.338,    # Branches
  X2 = 0.054,    # ATMs
  X3 = 6.899,    # Legal Rights
  X4 = -11.588,  # Regulation (High = Deregulation)
  X5 = 0.001     # GDP PPP
)

# Incarcare date pentru valorile medii de referinta
df <- readRDS(here("data", "processed_2", "panel_final_2.rds"))

# 2. Definire Scenarii ---------------------------------------------------------

# Scenariul A: Digitalizare (-20% sucursale)
delta_X1_A <- -0.20 * mean(df$X1, na.rm=TRUE)
impact_A <- delta_X1_A * coefs$X1

# Scenariul B: Reforma Legala (+2 puncte index)
delta_X3_B <- 2
impact_B <- delta_X3_B * coefs$X3

# Scenariul C: Liberalizare (+1 punct index Fraser - Dereglementare)
delta_X4_C <- 1
impact_C <- delta_X4_C * coefs$X4

# 3. Agregare Rezultate --------------------------------------------------------
scenarios_df <- tibble(
  Scenariu = c("A. Digitalizare (-20% Sucursale)", 
               "B. Reforma Legala (+2 pct)", 
               "C. Liberalizare (+1 pct)"),
  Delta_Input = c(paste0(round(delta_X1_A, 1), " unit/100k"), "+2.0", "+1.0"),
  Impact_Y_pp = c(impact_A, impact_B, impact_C),
  Tip_Efect = c("Neutral/Marginal", "Pozitiv Major", "Negativ (Disintermediere)")
)

# 4. Vizualizare ---------------------------------------------------------------
p_scen <- ggplot(scenarios_df, aes(x = reorder(Scenariu, Impact_Y_pp), y = Impact_Y_pp, fill = Impact_Y_pp > 0)) +
  geom_col(alpha = 0.8, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c("#E74C3C", "#27AE60"), guide = "none") +
  geom_text(aes(label = round(Impact_Y_pp, 2)), hjust = ifelse(scenarios_df$Impact_Y_pp > 0, -0.2, 1.2), fontface = "bold") +
  labs(title = "Simulare Impact Politici asupra Depozitelor (% PIB)",
       subtitle = "Estimari bazate pe modelul Fixed Effects (ceteris paribus)",
       x = NULL, y = "Modificare Estimata (puncte procentuale)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(here("output", "figures_2", "scenario_impact.png"), p_scen, width = 9, height = 5)

print(scenarios_df)
write_csv(scenarios_df, here("output", "tables_2", "scenarios_results.csv"))