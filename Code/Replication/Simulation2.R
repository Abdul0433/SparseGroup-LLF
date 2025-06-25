set.seed(12345)


source("efficiency_controls.R")


sim_data <- function(n, d, sigma) {
  X <- matrix(runif(n * d, 0, 1), nrow = n, ncol = d)
  y <- log(1 + exp(6 * X[, 1])) + rnorm(n, sd = sigma)
  list(X = X, y = y)
}

ds     <- c(5, 50)
ns     <- c(1000, 5000)
sigmas <- c(0.1, 1, 2)
test_n <-  1000
reps   <-  50

results <- expand.grid(d = ds, n = ns, sigma = sigmas, rep = 1:reps)
results[, c("RF", "LassoRF", "LLF", "BART", "XGB")] <- NA

for (i in seq_len(nrow(results))) {
  d     <- results$d[i]
  n     <- results$n[i]
  sigma <- results$sigma[i]
  
  train <- sim_data(n, d, sigma)
  test  <- sim_data(test_n, d, sigma)
  
  X <- train$X; Y <- train$y
  X.test <- test$X;  truth <- test$y
  
  # Random Forest
  rf_mod  <- regression_forest(X, Y, honesty = TRUE, tune.parameters = tune)
  pred_rf <- predict(rf_mod, X.test)$predictions
  results$RF[i] <- sqrt(mean((pred_rf - truth)^2))
  
  # Residual-on-split Lasso-RF
  inds <- sample(nrow(X), nrow(X) / 2)
  cv_las <- cv.glmnet(X[inds, ], Y[inds], alpha = 1)
  preds_las <- predict(cv_las, newx = X[-inds, ], s = "lambda.min")
  resid_rf <- Y[-inds] - as.numeric(preds_las)
  rf_las <- regression_forest(X[-inds, ], resid_rf, honesty = TRUE, tune.parameters = tune)
  pred_rf <- predict(rf_las, X.test)$predictions
  pred_las <- predict(cv_las, newx = X.test, s = "lambda.min")
  pred_lasrf <- as.numeric(pred_las) + pred_rf
  results$LassoRF[i] <- sqrt(mean((pred_lasrf - truth)^2))
  
  # Local Linear Forest
  llf_mod <- ll_regression_forest(X, Y, honesty = TRUE,
                                  enable.ll.split = TRUE,
                                  ll.split.weight.penalty = TRUE,
                                  tune.parameters = "none")
  
  cv_lasso_llf <- cv.glmnet(X, Y, alpha = 1)
  lasso_coef <- coef(cv_lasso_llf, s = "lambda.min")
  selected_names <- rownames(lasso_coef)[which(lasso_coef != 0)]
  selected_names <- setdiff(selected_names, "(Intercept)")
  
  if (length(selected_names) == 0) {
    selected_llf <- 1:ncol(X)
  } else {
    selected_llf <- match(selected_names, colnames(X))
    selected_llf <- selected_llf[!is.na(selected_llf)]  
    if (length(selected_llf) == 0) selected_llf <- 1:ncol(X)
  }
  print(selected_llf)  
  
  pred_llf <- predict(llf_mod, X.test,
                      linear.correction.variables = selected_llf,
                      ll.lambda = ll.lambda,
                      ll.weight.penalty = TRUE)$predictions
  
  results$LLF[i] <- sqrt(mean((pred_llf - truth)^2))
  
  
  #BART
  bart_mod <- wbart(X, Y, X.test, ndpost = ndpost)
  results$BART[i] <- sqrt(mean((bart_mod$yhat.test.mean - truth)^2))
  
  #XGBoost
  xgb_mod  <- rlearner::cvboost(X, Y,
                                ntrees_max = xgb_max,
                                num_search_rounds = num_search_rounds)
  pred_xgb <- predict(xgb_mod, X.test)
  results$XGB[i] <- sqrt(mean((pred_xgb - truth)^2))
}


sketch <- results %>%
  group_by(d, n, sigma) %>%
  summarise(
    RF      = mean(RF),
    LassoRF = mean(LassoRF),
    LLF     = mean(LLF),
    BART    = mean(BART),
    XGBoost = mean(XGB),
    .groups = "drop"
  ) %>%
  arrange(d, n, sigma)

dir.create("results", showWarnings = FALSE)
write.csv(sketch, "results/appendix_table2.csv", row.names = FALSE)
print(sketch)
        