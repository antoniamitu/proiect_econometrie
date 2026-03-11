# ==============================================================================
# SCRIPT: generate_descriptive_table_simple.R
# SCOP: Generarea Table 1 pentru articolul ICBE 2026
# VERSIUNE SIMPLIFICATĂ (fără flextable)
# ==============================================================================

# 1. Pachete necesare (doar cele de bază) ---------------------------------------
library(tidyverse)
library(moments)

# 2. Importarea datelor ---------------------------------------------------------
# MODIFICĂ ACEST PATH pentru locația ta
df <- read.csv("data/processed/train_data_raw.csv")

# Verificare rapidă
cat("Număr observații:", nrow(df), "\n")
cat("Variabile:", names(df), "\n\n")

# 3. Calcularea statisticilor ---------------------------------------------------
vars <- c("Y", "X1", "X2", "X3", "X4", "X5")

calc_stats <- function(x) {
  c(
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE)
  )
}

stats_matrix <- sapply(df[vars], calc_stats)
stats_df <- as.data.frame(t(stats_matrix))
stats_df$Variable <- rownames(stats_df)

# 4. Descrierile variabilelor ---------------------------------------------------
descriptions <- c(
  Y = "Outstanding deposits (% of GDP)",
  X1 = "Bank branches per 100,000 adults",
  X2 = "ATMs per 100,000 adults",
  X3 = "Legal System & Property Rights (0-10)",
  X4 = "Regulation (0-10)",
  X5 = "GDP per capita, PPP ($)",
  D = "High Economic Freedom dummy"
)

# 5. Adăugăm D ------------------------------------------------------------------
# Verifică cum e codat D în datele tale
if ("D" %in% names(df)) {
  d_numeric <- as.numeric(df$D == "HighFreedom" | df$D == 1 | df$D == TRUE)
  d_row <- data.frame(
    N = sum(!is.na(d_numeric)),
    Mean = mean(d_numeric, na.rm = TRUE),
    SD = sd(d_numeric, na.rm = TRUE),
    Min = 0,
    Max = 1,
    Skewness = NA,
    Variable = "D"
  )
  stats_df <- bind_rows(stats_df, d_row)
}

# 6. Formatare finală -----------------------------------------------------------
table1 <- stats_df %>%
  mutate(Description = descriptions[Variable]) %>%
  select(Variable, Description, N, Mean, SD, Min, Max, Skewness) %>%
  mutate(
    Mean = ifelse(Variable == "X5", 
                  format(round(Mean, 0), big.mark = ",", scientific = FALSE),
                  sprintf("%.2f", Mean)),
    SD = ifelse(Variable == "X5",
                format(round(SD, 0), big.mark = ",", scientific = FALSE),
                sprintf("%.2f", SD)),
    Min = ifelse(Variable == "X5",
                 format(round(Min, 0), big.mark = ",", scientific = FALSE),
                 sprintf("%.2f", Min)),
    Max = ifelse(Variable == "X5",
                 format(round(Max, 0), big.mark = ",", scientific = FALSE),
                 sprintf("%.2f", Max)),
    Skewness = ifelse(is.na(Skewness), "-", sprintf("%.2f", Skewness))
  )

# 7. OUTPUT 1: Consolă (formatat frumos) ----------------------------------------
cat("\n")
cat(strrep("=", 100), "\n")
cat("Table 1. Descriptive statistics (N = 111)\n")
cat(strrep("=", 100), "\n\n")

# Header
cat(sprintf("%-8s %-42s %6s %10s %10s %10s %10s %9s\n",
            "Variable", "Description", "N", "Mean", "Std.Dev.", "Min", "Max", "Skewness"))
cat(strrep("-", 100), "\n")

# Rows
for (i in 1:nrow(table1)) {
  cat(sprintf("%-8s %-42s %6s %10s %10s %10s %10s %9s\n",
              table1$Variable[i],
              substr(table1$Description[i], 1, 42),
              table1$N[i],
              table1$Mean[i],
              table1$SD[i],
              table1$Min[i],
              table1$Max[i],
              table1$Skewness[i]))
}

cat(strrep("-", 100), "\n")
cat("Source: Authors' calculations based on IMF FAS, Fraser Institute EFW, World Bank WDI,\n")
cat("and Heritage Foundation (all 2022 data).\n")
cat(strrep("=", 100), "\n")

# 8. OUTPUT 2: CSV (pentru import în Word) --------------------------------------
write.csv(table1, "Table1_Descriptive_Statistics.csv", row.names = FALSE)
cat("\n✓ Salvat: Table1_Descriptive_Statistics.csv\n")
cat("  → Deschide în Excel, copiază, și lipește în Word cu Paste Special > Table\n")

# 9. OUTPUT 3: Format pentru copy-paste direct în Word --------------------------
cat("\n\n=== COPY-PASTE DIRECT ÎN WORD (Tab-separated) ===\n\n")
cat("Variable\tDescription\tMean\tStd. Dev.\tMin\tMax\tSkewness\n")
for (i in 1:nrow(table1)) {
  cat(sprintf("%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
              table1$Variable[i],
              table1$Description[i],
              table1$Mean[i],
              table1$SD[i],
              table1$Min[i],
              table1$Max[i],
              table1$Skewness[i]))
}

cat("\n=== INSTRUCȚIUNI ===\n")
cat("1. Selectează textul tab-separated de mai sus\n")
cat("2. Copiază (Ctrl+C)\n")
cat("3. În Word: Insert > Table > Convert Text to Table\n")
cat("4. Separate text at: Tabs\n")
cat("5. OK\n")
