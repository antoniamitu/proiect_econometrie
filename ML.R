---
title: "Analiză Econometrică: Machine Learning (Local Data)"
author: "Proiect Econometrie"
date: "`r Sys.Date()`"
output: 
  html_document:
    theme: cerulean
    toc: true
    toc_float: true
    highlight: tango
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.align = "center")
# Funcție pentru verificarea și instalarea pachetelor lipsă
install_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Lista pachetelor necesare
packages <- c("tidyverse", "caret", "glmnet", "randomForest", "gbm", 
              "e1071", "cluster", "factoextra", "kableExtra", "gridExtra")

install_load(packages)

# Setăm seed-ul pentru reproductibilitate
set.seed(42)
# CONFIGURARE CALE:
DATA_DIR <- "data" 

# Lista numelor de fișiere
files_list <- list(
  raw_train   = "train_data_raw.csv",
  raw_test    = "test_data_raw.csv",
  log_train   = "train_data_log.csv",
  log_test    = "test_data_log.csv",
  pca_train   = "train_data_pca.csv",
  pca_test    = "test_data_pca.csv",
  clust_train = "train_data_clusters.csv",
  clust_test  = "test_data_clusters.csv"
)

# Funcție de citire sigură (caută și în folderul curent dacă nu găsește în subfolder)
read_local_data <- function(filename, path) {
  full_path <- file.path(path, filename)
  
  if (file.exists(full_path)) {
    return(read_csv(full_path, show_col_types = FALSE))
  } else if (file.exists(filename)) {
    # Fallback: poate fișierul e chiar lângă .Rmd
    return(read_csv(filename, show_col_types = FALSE))
  } else {
    stop(paste("EROARE CRITICĂ: Nu găsesc fișierul:", filename, "\nVerifică locația fișierelor."))
  }
}

# Încărcare efectivă
datasets <- lapply(files_list, function(x) read_local_data(x, DATA_DIR))

# Confirmare dimensiuni
print("Datele au fost încărcate corect:")
sapply(datasets, dim)
run_all_models <- function(train_data, test_data, target_col, predictors, label) {
  
  # Eliminăm NA-uri pentru siguranță
  train_data <- na.omit(train_data)
  test_data  <- na.omit(test_data)
  
  X_train <- train_data[, predictors]
  y_train <- train_data[[target_col]]
  X_test  <- test_data[, predictors]
  y_test  <- test_data[[target_col]]
  
  # Matrici pentru glmnet
  x_train_mat <- as.matrix(X_train)
  x_test_mat  <- as.matrix(X_test)
  
  # Cross-Validation
  train_control <- trainControl(method = "cv", number = 5)
  
  results <- data.frame()
  
  # --- 1. Ridge ---
  cv_ridge <- cv.glmnet(x_train_mat, y_train, alpha = 0, nfolds = 5)
  pred_ridge <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = x_test_mat)
  
  # --- 2. Lasso ---
  cv_lasso <- cv.glmnet(x_train_mat, y_train, alpha = 1, nfolds = 5)
  pred_lasso <- predict(cv_lasso, s = cv_lasso$lambda.min, newx = x_test_mat)
  
  # --- 3. Elastic Net ---
  enet_model <- train(
    x = x_train_mat, y = y_train, method = "glmnet",
    trControl = train_control, tuneLength = 5
  )
  pred_enet <- predict(enet_model, x_test_mat)
  
  # --- 4. Random Forest ---
  rf_model <- train(
    x = X_train, y = y_train, method = "rf",
    trControl = train_control, tuneGrid = expand.grid(mtry = max(2, floor(sqrt(ncol(X_train)))))
  )
  pred_rf <- predict(rf_model, X_test)
  
  # --- 5. Gradient Boosting ---
  gbm_model <- train(
    x = X_train, y = y_train, method = "gbm",
    trControl = train_control, verbose = FALSE, tuneLength = 3
  )
  pred_gbm <- predict(gbm_model, X_test)
  
  # --- 6. SVR ---
  svr_model <- svm(x = X_train, y = y_train, kernel = "radial", cost = 10, gamma = 0.1)
  pred_svr <- predict(svr_model, X_test)
  
  # --- Colectare Rezultate ---
  preds_list <- list(
    "Ridge" = as.vector(pred_ridge), "Lasso" = as.vector(pred_lasso),
    "ElasticNet" = as.vector(pred_enet), "RandomForest" = as.vector(pred_rf),
    "GradientBoosting" = as.vector(pred_gbm), "SVR" = as.vector(pred_svr)
  )
  
  for (model_name in names(preds_list)) {
    p <- preds_list[[model_name]]
    
    # Calculăm metricile DIRECT aici (fără funcții externe)
    rmse_val <- sqrt(mean((y_test - p)^2))
    mae_val  <- mean(abs(y_test - p))
    mape_val <- mean(abs((y_test - p) / y_test)) * 100 
    r2_val   <- cor(y_test, p)^2
    
    results <- rbind(results, data.frame(
      Model = model_name, Dataset = label,
      RMSE = rmse_val, MAE = mae_val, MAPE = mape_val, R2 = r2_val
    ))
  }
  return(results)
}
raw_train <- datasets$raw_train
raw_test  <- datasets$raw_test

# Codificare variabila dummy
raw_train$D_bin <- ifelse(raw_train$D == "HighFreedom", 1, 0)
raw_test$D_bin  <- ifelse(raw_test$D == "HighFreedom", 1, 0)

features_raw <- c("X1", "X2", "X3", "X4", "X5", "D_bin")
target_raw   <- "Y"

results_raw <- run_all_models(raw_train, raw_test, target_raw, features_raw, "RAW")

results_raw %>%
  kbl(caption = "Rezultate ML - Set RAW", digits = 2) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
log_train <- datasets$log_train
log_test  <- datasets$log_test

log_train$D_bin <- ifelse(log_train$D == "HighFreedom", 1, 0)
log_test$D_bin  <- ifelse(log_test$D == "HighFreedom", 1, 0)

features_log <- c("l_X1", "l_X2", "X3", "X4", "l_X5", "D_bin")
target_log   <- "l_Y"

results_log <- run_all_models(log_train, log_test, target_log, features_log, "LOG (log-scale)")

results_log %>%
  kbl(caption = "Rezultate ML - Set LOG (Scară logaritmică)", digits = 4) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# Pregătire date
x_train_log <- as.matrix(log_train[, features_log])
y_train_log <- log_train[[target_log]]
x_test_log  <- as.matrix(log_test[, features_log])
y_test_real <- log_test$Y 

# Antrenare Ridge manual
cv_ridge_log <- cv.glmnet(x_train_log, y_train_log, alpha = 0)
pred_log_scale <- predict(cv_ridge_log, s = cv_ridge_log$lambda.min, newx = x_test_log)

# Revenire la nivel (exponentiere)
pred_level <- exp(pred_log_scale)

# Calcul erori reale DIRECT (fără funcție externă)
ridge_log_rmse <- sqrt(mean((y_test_real - pred_level)^2))
ridge_log_mape <- mean(abs((y_test_real - pred_level) / y_test_real)) * 100

cat(paste0("Ridge LOG (revenit la nivel):\nRMSE: ", round(ridge_log_rmse, 2), 
           "\nMAPE: ", round(ridge_log_mape, 2), "%"))
plot_data <- data.frame(
  Actual = y_test_real,
  Predicted = as.vector(pred_level)
)
plot_data$Residuals <- plot_data$Actual - plot_data$Predicted

p1 <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Ridge LOG: Actual vs Predicted", x = "Valori Reale (Y)", y = "Predicții (Y)") +
  theme_minimal()

p2 <- ggplot(plot_data, aes(x = Residuals)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribuția Reziduurilor", x = "Eroare", y = "Frecvență") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)
clust_train <- datasets$raw_train
clust_test  <- datasets$raw_test

cols_clust <- c("X1", "X2", "X3", "X4", "X5")

# Scalare date
preproc <- preProcess(clust_train[, cols_clust], method = c("center", "scale"))
train_scaled <- predict(preproc, clust_train[, cols_clust])
test_scaled  <- predict(preproc, clust_test[, cols_clust])

# K-Means Clustering
set.seed(42)
km_res <- kmeans(train_scaled, centers = 3, nstart = 25)

# Adăugare cluster în train
clust_train$Cluster <- km_res$cluster

# Adăugare cluster în test (cel mai apropiat centru)
closest_cluster <- function(x, centers) {
  apply(x, 1, function(row) which.min(colSums((t(centers) - row)^2)))
}
clust_test$Cluster <- closest_cluster(test_scaled, km_res$centers)

# Rulare Modele
features_clust <- c("X1", "X2", "X3", "X4", "X5", "Cluster")
target_clust <- "Y"

results_clust <- run_all_models(clust_train, clust_test, target_clust, features_clust, "CLUSTERS")

results_clust %>%
  kbl(caption = "Rezultate ML - Set CLUSTERS", digits = 2) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
pca_train <- datasets$pca_train
pca_test  <- datasets$pca_test

pca_train$D_bin <- ifelse(pca_train$D == "HighFreedom", 1, 0)
pca_test$D_bin  <- ifelse(pca_test$D == "HighFreedom", 1, 0)

features_pca <- c("PC1", "PC2", "PC3", "PC4", "D_bin")
target_pca   <- "l_Y"

results_pca <- run_all_models(pca_train, pca_test, target_pca, features_pca, "PCA (log-scale)")

results_pca %>%
  kbl(caption = "Rezultate ML - Set PCA", digits = 4) %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# Unirea tuturor rezultatelor
all_results <- rbind(results_raw, results_log, results_clust, results_pca)

# Sortare după RMSE
final_table <- all_results %>% arrange(RMSE)

# Tabel Final - Top 10
final_table %>%
  head(10) %>%
  kbl(caption = "Top 10 Cele mai bune modele (RMSE minim)", digits = 4) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  row_spec(1, bold = T, color = "white", background = "darkgreen")

# Grafic RMSE
ggplot(all_results, aes(x = reorder(paste(Model, Dataset, sep=" - "), -RMSE), y = RMSE, fill = Dataset)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Performanța Modelelor (RMSE)",
       subtitle = "RMSE mai mic este mai bun",
       x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(legend.position = "bottom")
