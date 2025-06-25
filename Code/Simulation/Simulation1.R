# This script performs a Monte Carlo simulation to evaluate the out-of-sample RMSE
# of various machine learning models (LLF and its variants, RF, BART, XGBoost)
# under different conditions of sample size, feature dimensionality, and heteroscedasticity.

library(rugarch)
library(grf)
library(glmnet)
library(SGL)
library(dbarts)
library(xgboost)
library(Metrics)
library(progress)
library(tidyverse)
library(MCS)
set.seed(123)

make_random_spec <- function() {
  ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "norm",
    fixed.pars = list(
      omega = runif(1, 1e-6, 1e-4),
      alpha1 = runif(1, 0.05, 0.1),
      beta1 = runif(1, 0.8, 0.95),
      gamma1 = runif(1, -0.1, 0.1)
    )
  )
}

simulate_data <- function(n, beta, sigma_scale) {
  spec <- make_random_spec()
  path <- ugarchpath(spec, n.sim = n, n.start = 0, m.sim = 1)
  sigma_t <- as.numeric(sigma(path))
  eps <- rnorm(n, sd = sigma_scale * sigma_t)
  
  d <- length(beta)
  X <- matrix(rnorm(n * d), n, d)
  y <- X %*% beta + eps
  list(X = X, y = y)
}

evaluate_models <- function(X_train, y_train, X_test, y_test) {
  llf <- ll_regression_forest(
    X_train, y_train,
    honesty = TRUE,
    enable.ll.split = TRUE,
    ll.split.weight.penalty = TRUE,
    num.trees = 200,
    tune.parameters = "none"
  )
  y_llf <- predict(llf, X_test)$predictions
  
  lmod <- cv.glmnet(X_train, y_train, alpha = 1)
  coef_lasso <- coef(lmod, s = "lambda.min")[-1]
  idx_lasso <- which(as.numeric(coef_lasso) != 0)
  if (length(idx_lasso) == 0) idx_lasso <- seq(ncol(X_train))
  y_lassollf <- predict(
    llf, X_test,
    linear.correction.variables = idx_lasso,
    ll.lambda = 0.1
  )$predictions
  
  sgl_mod <- SGL(
    data = list(x = X_train, y = y_train),
    index = seq(ncol(X_train)),
    type = "linear",
    alpha = 0.95,
    standardize = TRUE
  )
  beta_sgl <- sgl_mod$beta[, which.min(sgl_mod$lambdas)]
  idx_sgl <- which(beta != 0)
  if (length(idx_sgl) == 0) idx_sgl <- seq(ncol(X_train))
  y_sgl <- predict(
    llf, X_test,
    linear.correction.variables = idx_sgl,
    ll.lambda = 0.1
  )$predictions
  
  rf <- regression_forest(X_train, y_train, honesty = TRUE, num.trees = 200)
  y_rf <- predict(rf, X_test)$predictions
  
  bart_mod <- bart(
    x.train = X_train, y.train = y_train,
    x.test = X_test, verbose = FALSE
  )
  y_bart <- bart_mod$yhat.test.mean
  
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_mod <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = 100,
    verbose = 0
  )
  y_xgb <- predict(xgb_mod, xgb.DMatrix(data = X_test))
  
  tibble(
    model = c("LLF", "Lasso-LLF", "SGL-LLF", "RF", "BART", "XGB"),
    RMSE = c(
      rmse(y_test, y_llf),
      rmse(y_test, y_lassollf),
      rmse(y_test, y_sgl),
      rmse(y_test, y_rf),
      rmse(y_test, y_bart),
      rmse(y_test, y_xgb)
    )
  )
}

sample_sizes <- c(250, 500, 1000)
d_vals <- c(5, 20, 40)
sigma_vals <- c(1, 5, 10)
n_reps <- 1 #10

total_iters <- length(sample_sizes) *
  length(d_vals) *
  length(sigma_vals) *
  n_reps

pb <- progress_bar$new(
  total = total_iters,
  format = "  Simulatie [:bar] :percent in :elapsed"
)

results <- vector("list", length = total_iters)
ct <- 1

for (n in sample_sizes) {
  for (d in d_vals) {
    beta <- c(rep(1, 5), rep(0, d - 5))
    for (sigma_scale in sigma_vals) {
      for (r in seq_len(n_reps)) {
        sim <- simulate_data(n, beta, sigma_scale)
        idx_tr <- seq_len(floor(0.8 * n))
        idx_te <- (floor(0.8 * n) + 1L):n
        
        res <- evaluate_models(
          sim$X[idx_tr,], sim$y[idx_tr],
          sim$X[idx_te,], sim$y[idx_te]
        ) %>%
          mutate(
            n = n,
            d = d,
            sigma = sigma_scale,
            rep = r
          )
        
        results[[ct]] <- res
        ct <- ct + 1
        pb$tick()
      }
    }
  }
}

final_results <- bind_rows(results)
agg_results <- final_results %>%
  group_by(n, d, sigma, model) %>%
  summarise(mean_RMSE = mean(RMSE), .groups = "drop")

print(agg_results)

# write_xlsx(agg_results, "C:/Users/abdul/Documents/Extension_Thesis/Results/Simulations/Simulation_1.xlsx")

# --- MODEL CONFIDENCE SET – zonder eliminatie -------------------------------
# alpha extreem klein ⇒ p-waarde < alpha vrijwel onmogelijk ⇒ geen modellen weg
LossMat <- final_results %>%
  unite(block, n, d, sigma, rep, remove = FALSE) %>%
  select(block, model, RMSE) %>%
  pivot_wider(names_from = model, values_from = RMSE) %>%
  select(-block) %>% 
  as.matrix()

mcs_obj <- MCSprocedure(
  Loss      = LossMat,
  alpha     = 0.05,     # praktisch nul ⇒ alles blijft in de set
  B         = 5000,
  statistic = "Tmax",
  verbose   = FALSE
)


# --- Volledige MCS‐samenvatting met alle modellen --------------------------
alpha_val <- 0.05
all_models <- c("LLF","Lasso-LLF","SGL-LLF","RF","BART","XGB")

# Haal p-waardes uit mcs_obj@show voor de overgebleven modellen
sup_models <- rownames(mcs_obj@show)
pvals <- setNames(
  sapply(all_models, function(m) {
    if (m %in% sup_models) mcs_obj@show[m, "MCS_M"] else 0
  }),
  all_models
)

# Indicator in de superior set
in_mcs <- as.integer(pvals > alpha_val)

# Bouw df
mcs_full <- tibble(
  Model    = all_models,
  alpha    = alpha_val,
  p_value  = unname(pvals),
  in_MCS   = in_mcs
)

cat("\n--- Complete MCS-resultaten (α =", alpha_val, ") ---\n")
print(mcs_full)
mcs_obj@show
