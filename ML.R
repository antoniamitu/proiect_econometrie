# =========================
# ML pipeline

# =========================

# ---------- Packages ----------
install_load <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
  invisible(sapply(pkgs, require, character.only = TRUE))
}

packages <- c(
  "tidyverse", "caret", "glmnet", "randomForest", "gbm",
  "e1071", "cluster", "kernlab", "ggplot2"
)
install_load(packages)

set.seed(42)

# ---------- Config ----------
DATA_DIR <- file.path("data", "processed")

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

read_local_data <- function(filename, path) {
  full_path <- file.path(path, filename)
  if (file.exists(full_path)) {
    return(readr::read_csv(full_path, show_col_types = FALSE))
  } else if (file.exists(filename)) {
    return(readr::read_csv(filename, show_col_types = FALSE))
  } else {
    stop(paste("EROARE: Nu găsesc fișierul:", filename))
  }
}

datasets <- lapply(files_list, function(x) read_local_data(x, DATA_DIR))
message("Loaded datasets dims:")
print(sapply(datasets, dim))

# ---------- Helpers ----------
standardize_train_test <- function(X_train, X_test) {
  X_train <- as.matrix(X_train)
  X_test  <- as.matrix(X_test)
  
  mu <- colMeans(X_train)
  sdv <- apply(X_train, 2, sd)
  sdv[sdv == 0] <- 1
  
  X_train_s <- sweep(sweep(X_train, 2, mu, "-"), 2, sdv, "/")
  X_test_s  <- sweep(sweep(X_test, 2, mu, "-"), 2, sdv, "/")
  
  list(X_train_s = X_train_s, X_test_s = X_test_s)
}

safe_mape <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true); y_pred <- as.numeric(y_pred)
  idx <- which(!is.na(y_true) & !is.na(y_pred) & y_true != 0)
  if (length(idx) == 0) return(NA_real_)
  mean(abs((y_true[idx] - y_pred[idx]) / y_true[idx])) * 100
}

rmse <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))
mae  <- function(y_true, y_pred) mean(abs(y_true - y_pred), na.rm = TRUE)

r2_sse <- function(y_true, y_pred) {
  y_true <- as.numeric(y_true); y_pred <- as.numeric(y_pred)
  ss_res <- sum((y_true - y_pred)^2, na.rm = TRUE)
  ss_tot <- sum((y_true - mean(y_true, na.rm = TRUE))^2, na.rm = TRUE)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

metrics_tbl <- function(y_true, y_pred) {
  tibble::tibble(
    RMSE = rmse(y_true, y_pred),
    MAE  = mae(y_true, y_pred),
    MAPE = safe_mape(y_true, y_pred),
    R2   = r2_sse(y_true, y_pred)
  )
}

# ---------- Core runner ----------
run_all_models <- function(
    train_data, test_data,
    target_col, predictors,
    label,
    is_log_target = FALSE,
    target_level_col = NULL,
    is_log1p = FALSE
) {
  needed_cols <- unique(c(predictors, target_col, target_level_col))
  
  train_data <- train_data %>% dplyr::select(all_of(needed_cols)) %>% stats::na.omit()
  test_data  <- test_data  %>% dplyr::select(all_of(needed_cols)) %>% stats::na.omit()
  
  X_train <- train_data[, predictors, drop = FALSE]
  y_train <- train_data[[target_col]]
  X_test  <- test_data[, predictors, drop = FALSE]
  y_test  <- test_data[[target_col]]
  
  x_train_mat <- as.matrix(X_train)
  x_test_mat  <- as.matrix(X_test)
  
  ctrl <- caret::trainControl(method = "cv", number = 5)
  
  preds <- list()
  
  # 1) Ridge
  cv_ridge <- glmnet::cv.glmnet(x_train_mat, y_train, alpha = 0, nfolds = 5, standardize = TRUE)
  preds$Ridge <- as.numeric(predict(cv_ridge, s = cv_ridge$lambda.min, newx = x_test_mat))
  
  # 2) Lasso
  cv_lasso <- glmnet::cv.glmnet(x_train_mat, y_train, alpha = 1, nfolds = 5, standardize = TRUE)
  preds$Lasso <- as.numeric(predict(cv_lasso, s = cv_lasso$lambda.min, newx = x_test_mat))
  
  # 3) Elastic Net 
  enet_grid <- expand.grid(
    alpha = seq(0.1, 1.0, by = 0.1),
    lambda = 10^seq(-4, 2, length.out = 30)
  )
  enet_fit <- caret::train(
    x = x_train_mat, y = y_train,
    method = "glmnet",
    trControl = ctrl,
    tuneGrid = enet_grid,
    preProcess = c("center", "scale")
  )
  preds$ElasticNet <- as.numeric(predict(enet_fit, newdata = x_test_mat))
  
  # 4) Random Forest 
  p <- ncol(X_train)
  rf_grid <- expand.grid(mtry = unique(pmax(1, floor(c(sqrt(p), p/3, p/2)))))
  rf_fit <- caret::train(
    x = X_train, y = y_train,
    method = "rf",
    trControl = ctrl,
    tuneGrid = rf_grid
  )
  preds$RandomForest <- as.numeric(predict(rf_fit, newdata = X_test))
  
  # 5) Gradient Boosting 
  gbm_fit <- caret::train(
    x = X_train, y = y_train,
    method = "gbm",
    trControl = ctrl,
    verbose = FALSE,
    tuneLength = 5
  )
  preds$GradientBoosting <- as.numeric(predict(gbm_fit, newdata = X_test))
  
  # 6) SVR 
  svr_grid <- expand.grid(
    sigma = c(0.01, 0.05, 0.1),
    C = c(1, 10, 100)
  )
  svr_fit <- caret::train(
    x = X_train, y = y_train,
    method = "svmRadial",
    trControl = ctrl,
    tuneGrid = svr_grid,
    preProcess = c("center", "scale")
  )
  preds$SVR <- as.numeric(predict(svr_fit, newdata = X_test))
  
  # results on target scale (Y or l_Y)
  res_target <- purrr::imap_dfr(preds, function(p_hat, model_name) {
    metrics_tbl(y_test, p_hat) %>%
      dplyr::mutate(Model = model_name, Dataset = label, Scale = target_col) %>%
      dplyr::select(Model, Dataset, Scale, dplyr::everything())
  })
  
  # results on level scale if log-target 
  res_level <- tibble::tibble()
  if (is_log_target && !is.null(target_level_col)) {
    y_level <- test_data[[target_level_col]]
    
    inv_log <- function(x) {
      if (is_log1p) return(expm1(x))
      exp(x)
    }
    
    res_level <- purrr::imap_dfr(preds, function(p_hat_log, model_name) {
      p_level <- as.numeric(inv_log(p_hat_log))
      metrics_tbl(y_level, p_level) %>%
        dplyr::mutate(Model = model_name, Dataset = label, Scale = target_level_col) %>%
        dplyr::select(Model, Dataset, Scale, dplyr::everything())
    })
  }
  
  dplyr::bind_rows(res_target, res_level)
}

# =========================
# RAW
# =========================
raw_train <- datasets$raw_train
raw_test  <- datasets$raw_test

raw_train$D_bin <- ifelse(raw_train$D == "HighFreedom", 1, 0)
raw_test$D_bin  <- ifelse(raw_test$D == "HighFreedom", 1, 0)

features_raw <- c("X1", "X2", "X3", "X4", "X5", "D_bin")
target_raw <- "Y"

results_raw <- run_all_models(
  raw_train, raw_test,
  target_col = target_raw,
  predictors = features_raw,
  label = "RAW",
  is_log_target = FALSE
)
print(results_raw)

# =========================
# LOG 
# =========================
log_train <- datasets$log_train
log_test  <- datasets$log_test

if (!("Y" %in% names(log_train))) log_train$Y <- exp(log_train$l_Y)
if (!("Y" %in% names(log_test)))  log_test$Y  <- exp(log_test$l_Y) 

log_train$D_bin <- ifelse(log_train$D == "HighFreedom", 1, 0)
log_test$D_bin  <- ifelse(log_test$D == "HighFreedom", 1, 0)

features_log <- c("l_X1", "l_X2", "X3", "X4", "l_X5", "D_bin")
target_log <- "l_Y"

results_log <- run_all_models(
  log_train, log_test,
  target_col = target_log,
  predictors = features_log,
  label = "LOG",
  is_log_target = TRUE,
  target_level_col = "Y",
  is_log1p = FALSE  
)
print(results_log)

# =========================
# CLUSTERS 
# =========================
clust_train_data <- datasets$raw_train
clust_test_data  <- datasets$raw_test

clustering_features <- c("X1", "X2", "X3", "X4", "X5")

sc_km <- standardize_train_test(
  clust_train_data[, clustering_features],
  clust_test_data[, clustering_features]
)

set.seed(42)
km_res <- stats::kmeans(sc_km$X_train_s, centers = 3, nstart = 10)

assign_cluster <- function(X_scaled, centers) {
  X_scaled <- as.matrix(X_scaled)
  centers <- as.matrix(centers)
  apply(X_scaled, 1, function(row) which.min(colSums((t(centers) - row)^2)))
}

clust_train_data$Cluster <- assign_cluster(sc_km$X_train_s, km_res$centers)
clust_test_data$Cluster  <- assign_cluster(sc_km$X_test_s,  km_res$centers)

# drop columns
clust_train <- clust_train_data %>% dplyr::select(-iso_code, -country, -D)
clust_test  <- clust_test_data  %>% dplyr::select(-iso_code, -country, -D)

features_clust <- c("X1", "X2", "X3", "X4", "X5", "Cluster")
target_clust <- "Y"

results_clust <- run_all_models(
  clust_train, clust_test,
  target_col = target_clust,
  predictors = features_clust,
  label = "CLUSTERS",
  is_log_target = FALSE
)
print(results_clust)
warnings()
# =========================
# PCA 
# =========================
pca_train <- datasets$pca_train
pca_test  <- datasets$pca_test

if (!("Y" %in% names(pca_train))) pca_train$Y <- exp(pca_train$l_Y)   
if (!("Y" %in% names(pca_test)))  pca_test$Y  <- exp(pca_test$l_Y)

pca_train$D_bin <- ifelse(pca_train$D == "HighFreedom", 1, 0)
pca_test$D_bin  <- ifelse(pca_test$D == "HighFreedom", 1, 0)

features_pca <- c("PC1", "PC2", "PC3", "PC4", "D_bin")
target_pca <- "l_Y"

results_pca <- run_all_models(
  pca_train, pca_test,
  target_col = target_pca,
  predictors = features_pca,
  label = "PCA",
  is_log_target = TRUE,
  target_level_col = "Y",
  is_log1p = FALSE  
)
print(results_pca)

# =========================
# FINAL(A)
# =========================
all_results_python_like <- dplyr::bind_rows(
  results_raw   %>% dplyr::filter(Scale == "Y"),
  results_log   %>% dplyr::filter(Scale == "l_Y"),
  results_clust %>% dplyr::filter(Scale == "Y"),
  results_pca   %>% dplyr::filter(Scale == "l_Y")
)

all_results_sorted <- all_results_python_like %>% dplyr::arrange(RMSE)

cat("\n=== GLOBAL RANKING (mixed scales: Y vs l_Y) ===\n")
print(all_results_sorted)

ggplot2::ggplot(all_results_sorted,
                ggplot2::aes(x = reorder(paste(Model, Dataset, sep=" - "), -RMSE),
                             y = RMSE, fill = Dataset)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Performanța modelelor (RMSE) - clasament global",
    subtitle = "Notă",
    x = "Model", y = "RMSE"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

# =========================
# FINAL B) OPTIONAL
# =========================
all_results_all <- dplyr::bind_rows(results_raw, results_log, results_clust, results_pca)
all_results_level <- all_results_all %>% dplyr::filter(Scale == "Y") %>% dplyr::arrange(RMSE)

cat("\n=== OPTIONAL: COMPARABLE RANKING ON Y (level) ONLY ===\n")
print(all_results_level)

