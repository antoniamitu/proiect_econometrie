# ==============================================================================
# PROIECT ECONOMETRIE 2025-2026
# APLICATIA 1: SCENARII DE PREDICTIE SI INTERVALE DE INCREDERE
#
# SCRIPT: a1_04_model_extins_scenarii.R
# RESPONSABIL: Student 1 (Analist) & Student 2 (Econometrician)
#
# OBIECTIVE:
# 1. Re-estimarea modelului optim (Log-Log) pe intreg setul de date (Train + Test).
# 2. Crearea unor scenarii ipotetice (ex: "Ce se intampla daca imbunatatim legislatia?").
# 3. Vizualizarea predictiilor cu intervale de incredere.
# ==============================================================================

# 1. Setup si Pachete ---------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stargazer")) install.packages("stargazer")
if (!require("ggplot2")) install.packages("ggplot2")

library(tidyverse)
library(stargazer)
library(ggplot2)

# Output directories
if (!dir.exists("output/predictions")) dir.create("output/predictions", recursive = TRUE)

# 2. Pregatirea Modelului Final ----------------------------------------------
# Re-antrenam modelul log-log pe TOATE datele disponibile (full_data_raw),
# folosind EXACT aceleasi transformari ca in a1_04_model_logaritmat.

df_full <- read.csv("data/processed/full_data_raw.csv")

df_final <- df_full %>%
  mutate(
    l_Y  = log(Y),
    l_X1 = log(X1),
    l_X2 = log(X2),
    l_X5 = log(X5),
    D    = as.factor(D)
  )

model_final <- lm(l_Y ~ l_X1 + l_X2 + X3 + X4 + l_X5 + D, data = df_final)
summary(model_final)

# 3. Definirea Scenariilor (What-If) ------------------------------------------

# "Tara medie" pe variabilele in nivel (ca in a1_04)
means <- df_final %>%
  summarise(
    X1 = mean(X1, na.rm = TRUE),
    X2 = mean(X2, na.rm = TRUE),
    X3 = mean(X3, na.rm = TRUE),
    X4 = mean(X4, na.rm = TRUE),
    X5 = mean(X5, na.rm = TRUE)
  )

scenarii <- data.frame(
  Scenariu = c(
    "1. Situatia Medie (Baseline)",
    "2. Reforma Legislativa (+2 pct X4)",
    "3. Crestere Economica (+20% PIB)"
  ),
  l_X1 = log(means$X1),
  l_X2 = log(means$X2),
  X3   = means$X3,
  X4   = c(means$X4, means$X4 + 2, means$X4),
  l_X5 = c(log(means$X5), log(means$X5), log(means$X5 * 1.2)),
  D    = factor("HighFreedom", levels = levels(df_final$D))
)

# 4. Predictii + Interval de Predictie (pe scara log, apoi expon.) -----------

pred <- predict(model_final, newdata = scenarii,
                interval = "prediction", level = 0.95)

rezultate_scenarii <- scenarii %>%
  bind_cols(as.data.frame(pred)) %>%
  mutate(
    Y_pred = exp(fit),
    Y_min  = exp(lwr),
    Y_max  = exp(upr)
  ) %>%
  select(Scenariu, Y_pred, Y_min, Y_max)

print(rezultate_scenarii)

write.csv(rezultate_scenarii,
          "output/predictions/a1_04_scenarii_economice.csv",
          row.names = FALSE)

# 5. Vizualizarea Scenariilor -----------------------------------------------

p_scenarii <- ggplot(rezultate_scenarii,
                     aes(x = Scenariu, y = Y_pred, fill = Scenariu)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = Y_min, ymax = Y_max),
                width = 0.2, size = 1) +
  geom_text(aes(label = round(Y_pred, 1)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Impactul Reformelor asupra Depozitelor Bancare",
    subtitle = "Predictii model Log-Log (interval de predictie 95%)",
    y        = "Depozite Bancare (% din PIB)",
    x        = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11))

ggsave("output/predictions/a1_04_plot_scenarii.png",
       plot = p_scenarii, width = 8, height = 6)

# 6. Predictie Specifica pentru Romania (daca exista in date) -----------------

romania_data <- df_final %>%
  filter(iso_code == "ROU" | country == "Romania")

if (nrow(romania_data) > 0) {
  pred_ro <- predict(model_final, newdata = romania_data,
                     interval = "confidence")
  
  valoare_reala   <- romania_data$Y
  valoare_prezisa <- exp(pred_ro[1, "fit"])
  
  cat("\n=== ANALIZA PENTRU ROMANIA ===\n")
  cat("Valoare Reala (Depozite % PIB):",
      round(valoare_reala, 2), "%\n")
  cat("Valoare Estimata de Model:     ",
      round(valoare_prezisa, 2), "%\n")
  
  if (valoare_reala < valoare_prezisa) {
    cat("Concluzie: Romania performeaza SUB potentialul indicat de model.\n")
  } else {
    cat("Concluzie: Romania performeaza PESTE potentialul indicat de model.\n")
  }
}