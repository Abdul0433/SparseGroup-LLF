# This script performs a rolling window analysis to forecast cryptocurrency volatility using various models,
# including Local Linear Forests, Random Forests, GARCH, HAR-RV, and XGBoost, and evaluates their performance.
rm(list = ls())

# Ensure reproducibility for base R and xgboost
dotR.seed <- 12345
set.seed(dotR.seed)

library(tidyverse)
library(grf)
library(glmnet)
library(rugarch)
library(progress)
library(Metrics)
library(SGL)
library(xgboost)
library(writexl)

make_controls <- function(efficient = TRUE,
                          days_train = 60,
                          days_forecast = 1,
                          var_threshold = 1e-6) {
  list(
    window_size = days_train,
    forecast_horizon = days_forecast,
    step_size = days_forecast,
    num_trees = if (efficient) 200 else 1000,
    ll_lambda = 0.1,
    sgl_alpha = 0.95,
    var_threshold = var_threshold,
    progress = TRUE,
    seed = dotR.seed
  )
}
ctrl <- make_controls(efficient = FALSE, days_train = 30, days_forecast = 1)

target <- "rv"
predictors <- c(
  "high_low_range", "lag_rv", "lag_return", "atr14", "ma7", "ma14", "ma30",
  "rsi14", "roc9", "mean_rv_5d", "sum_past_returns_3", "diff_past_returns_3",
  "volume", "weekend", "rv_month", "rv_week",
  "fear_greed", "vix", "sp500", "nasdaq", "gold", "google_trends"
)

df <- read_csv("C:/Users/abdul/Documents/Extension_Thesis/Data/Daily_data/BTCUSDT_daily_aggregated.csv") %>%
  drop_na(all_of(c(predictors, target, "day"))) %>%
  mutate(date = as.Date(day, format = "%d/%m/%Y"))
stopifnot(all(predictors %in% names(df)))

preprocess <- function(mat, threshold) {
  vars <- apply(mat, 2, var, na.rm = TRUE)
  keep <- names(vars[vars > threshold])
  if (length(keep) < 2) return(NULL)
  scaled <- scale(mat[, keep, drop = FALSE])
  list(scaled = scaled, cols = keep,
       center = attr(scaled, "scaled:center"),
       scale = attr(scaled, "scaled:scale"))
}

fit_llf_family <- function(X_train, y_train, X_test, params) {
  n <- nrow(X_test)
  llf <- tryCatch(
    grf::ll_regression_forest(X_train, y_train,
                              honesty = TRUE,
                              enable.ll.split = TRUE,
                              ll.split.weight.penalty = TRUE,
                              num.trees = params$num_trees,
                              tune.parameters = "none"),
    error = function(e) NULL)
  if (is.null(llf)) return(list(llf = rep(NA, n), lasso = rep(NA, n), sgl = rep(NA, n)))
  llf_pred <- predict(llf, X_test, linear.correction.variables = seq(ncol(X_train)), ll.lambda = params$ll_lambda)$predictions
  lmod <- tryCatch(glmnet::cv.glmnet(X_train, y_train, alpha = 1), error = function(e) NULL)
  lassop <- if (is.null(lmod)) rep(NA, n) else {
    coefn <- coef(lmod, s = "lambda.min")[-1]
    idx <- which(as.numeric(coefn) != 0)
    if (length(idx) == 0) rep(NA, n) else
      predict(llf, X_test, linear.correction.variables = idx, ll.lambda = params$ll_lambda)$predictions
  }
  sglp <- tryCatch({
    ids <- seq(ncol(X_train))
    fit <- SGL::SGL(data = list(x = X_train, y = y_train), index = ids,
                    type = "linear", alpha = params$sgl_alpha, standardize = TRUE)
    beta <- fit$beta[, which.min(fit$lambdas)]
    idx_sgl <- which(beta != 0)
    if (length(idx_sgl) == 0) rep(NA, n) else
      predict(llf, X_test, linear.correction.variables = idx_sgl, ll.lambda = params$ll_lambda)$predictions
  }, error = function(e) rep(NA, n))
  list(llf = llf_pred, lasso = lassop, sgl = sglp)
}

fit_rf <- function(X_train, y_train, X_test, params) {
  rf <- tryCatch(
    grf::regression_forest(X_train, y_train, honesty = TRUE, num.trees = params$num_trees),
    error = function(e) NULL)
  if (is.null(rf)) return(rep(NA, nrow(X_test)))
  predict(rf, X_test)$predictions
}

fit_garch <- function(y_train, h) {
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  )
  fit <- tryCatch(ugarchfit(spec = spec, data = y_train, solver = "hybrid"), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA, h))
  f <- tryCatch(ugarchforecast(fit, n.ahead = h), error = function(e) NULL)
  if (is.null(f)) return(rep(NA, h))
  as.numeric(rugarch::sigma(f))^2
}

fit_har <- function(train_df, test_df) {
  mod <- tryCatch(lm(rv ~ lag_rv + rv_week + rv_month, data = train_df), error = function(e) NULL)
  if (is.null(mod)) return(rep(NA, nrow(test_df)))
  predict(mod, newdata = test_df)
}

fit_xgb <- function(X_train, y_train, X_test, params) {
  set.seed(params$seed)
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgboost::xgb.DMatrix(data = X_test)
  model <- tryCatch(
    xgboost::xgboost(data = dtrain, objective = "reg:squarederror", nrounds = 50, verbose = 0),
    error = function(e) NULL
  )
  if (is.null(model)) return(rep(NA, nrow(X_test)))
  predict(model, dtest)
}

n <- nrow(df)
max_start <- n - ctrl$window_size - ctrl$forecast_horizon + 1
num_iter <- floor(max_start / ctrl$step_size) + 1
results_list <- vector("list", num_iter)
if (ctrl$progress) pb <- progress_bar$new(total = num_iter, format = "[:bar] :percent :etas")

for (i in seq_len(num_iter)) {
  start <- (i - 1) * ctrl$step_size + 1
  train_idx <- start:(start + ctrl$window_size - 1)
  test_idx <- (start + ctrl$window_size):(start + ctrl$window_size + ctrl$forecast_horizon - 1)
  if (max(test_idx) > n) break
  train_df <- df[train_idx, ]
  test_df <- df[test_idx, ]
  if (!target %in% names(train_df)) next
  prep <- preprocess(as.matrix(train_df[predictors]), ctrl$var_threshold)
  if (is.null(prep)) next
  X_train <- prep$scaled
  X_test <- scale(
    as.matrix(test_df[predictors])[, prep$cols, drop = FALSE],
    center = prep$center,
    scale = prep$scale
  )
  y_train <- train_df[[target]]
  y_test <- test_df[[target]]
  
  llf_out <- fit_llf_family(X_train, y_train, X_test, ctrl)
  rf_out <- fit_rf(X_train, y_train, X_test, ctrl)
  garch_out <- fit_garch(y_train, ctrl$forecast_horizon)
  har_out <- fit_har(train_df, test_df)
  xgb_out <- fit_xgb(X_train, y_train, X_test, ctrl)
  
  results_list[[i]] <- tibble(
    date = test_df$date,
    actual = y_test,
    LLF = llf_out$llf,
    LassoLLF = llf_out$lasso,
    SparseGroupLLF = llf_out$sgl,
    RF = rf_out,
    GARCH = garch_out,
    HARRV = har_out,
    XGBoost = xgb_out
  )
  
  if (ctrl$progress && i %% 10 == 0) pb$tick(10)
}

results <- bind_rows(results_list)
results_long <- results %>%
  pivot_longer(cols = -c(date, actual), names_to = "model", values_to = "forecast")

floor_val <- 1e-4
rmse_metrics <- results_long %>%
  group_by(model) %>%
  summarise(
    RMSE = Metrics::rmse(actual, pmax(forecast, floor_val)),
    .groups = "drop"
  )

qlike_metrics <- results_long %>%
  group_by(model) %>%
  summarise(
    QLIKE = mean(
      actual / pmax(forecast, floor_val) -
        log(actual / pmax(forecast, floor_val)) -
        1,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

agg_metrics <- left_join(rmse_metrics, qlike_metrics, by = "model")
print(agg_metrics)

write_xlsx(list(Results = results, Metrics = agg_metrics), path = "C:/Users/abdul/Documents/Extension_Thesis/Results/Forecast/rolling_results30_BTC.xlsx")

full_prep <- preprocess(as.matrix(df[predictors]), ctrl$var_threshold)
if (!is.null(full_prep)) {
  X_full <- full_prep$scaled
  y_full <- df[[target]]
  llf_full <- grf::ll_regression_forest(
    X_full, y_full,
    honesty = TRUE,
    enable.ll.split = TRUE,
    ll.split.weight.penalty = TRUE,
    num.trees = ctrl$num_trees,
    tune.parameters = "none"
  )
  vi_llf <- grf::variable_importance(llf_full)
  df_llf <- tibble(Feature = full_prep$cols, Importance = vi_llf, Model = "LLF")
  lmod_full <- glmnet::cv.glmnet(X_full, y_full, alpha = 1)
  coef_lasso <- abs(as.numeric(coef(lmod_full, s = "lambda.min")[-1]))
  df_lasso <- tibble(Feature = full_prep$cols, Importance = coef_lasso, Model = "LassoLLF")
  sgl_fit_full <- SGL::SGL(
    data = list(x = X_full, y = y_full),
    index = seq(ncol(X_full)),
    type = "linear", alpha = ctrl$sgl_alpha, standardize = TRUE
  )
  beta_sgl <- abs(sgl_fit_full$beta[, which.min(sgl_fit_full$lambdas)])
  df_sgl <- tibble(Feature = full_prep$cols, Importance = beta_sgl, Model = "SparseGroupLLF")
  df_imp <- bind_rows(df_llf, df_lasso, df_sgl) %>% filter(Importance > 0)
  plot_imp <- ggplot(df_imp, aes(x = reorder(Feature, Importance), y = Importance, fill = Model)) +
    geom_col(position = "dodge") + coord_flip() +
    labs(title = "Feature Importance for LLF Models-BTC", x = NULL, y = "Importance") +
    theme_minimal() + theme(legend.title = element_blank())
  print(plot_imp)
}