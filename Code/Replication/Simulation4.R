# create figure 4 
nsim <- 50
ntrain <- 600
ntest  <- 600
dims   <- seq(2, 20, by = 1)
results <- data.frame()

for (d in dims) {
  cat("Running dimension:", d, "\n")
  rmse_rf  <- numeric(nsim)
  rmse_llf_cart <- numeric(nsim)
  rmse_llf_ridge <- numeric(nsim)
  
  for (sim in 1:nsim) {
    X <- matrix(runif(ntrain * d), ntrain, d)
    X.test <- matrix(runif(ntest * d), ntest, d)
    yfun <- function(X) {
      n <- nrow(X)
      y <- 20 * (X[,1] - 0.5)^3
      if (d >= 2) y <- y + rowSums(10 * X[,2:pmin(3,d), drop=FALSE])
      if (d >= 4) y <- y + rowSums(5 * X[,4:pmin(5,d), drop=FALSE])
      if (d >= 6 && d > 5) y <- y + rowSums(2 * X[,6:d, drop=FALSE])
      y + rnorm(n, sd=1)
    }
    Y <- yfun(X)
    Y.test <- yfun(X.test)
    
    # RF
    rf_mod <- regression_forest(X, Y, honesty = TRUE, tune.parameters = tune)
    rf.preds <- predict(rf_mod, X.test)$predictions
    rmse_rf[sim] <- sqrt(mean((rf.preds - Y.test)^2))
    
    # LLF_CART
    llf_cart <- ll_regression_forest(X, Y, honesty = TRUE, enable.ll.split = FALSE)
    pred_llf_cart <- predict(llf_cart, X.test, ll.lambda = 0.1)$predictions
    rmse_llf_cart[sim] <- sqrt(mean((pred_llf_cart - Y.test)^2))
    
    # LLF_Residual (ridge splits)
    colnames(X) <- paste0("X", 1:d)
    colnames(X.test)  <- paste0("X", 1:d)
    lasso <- cv.glmnet(X, Y, alpha = 1)
    coef_sel <- coef(lasso, s = "lambda.min")
    sel_names <- rownames(coef_sel)[which(coef_sel != 0)]
    sel_names <- setdiff(sel_names, "(Intercept)")
    if (length(sel_names) == 0) {
      selected <- 1:d
    } else {
      selected <- match(sel_names, colnames(X))
      selected <- selected[!is.na(selected)]
      if (length(selected) == 0) selected <- 1:d
    }
    llf_ridge <- ll_regression_forest(
      X, Y,
      honesty = TRUE,
      enable.ll.split = TRUE,
      ll.split.weight.penalty = TRUE
    )
    pred_llf_ridge <- predict(
      llf_ridge, X.test,
      linear.correction.variables = selected,
      ll.lambda = 0.1,
      ll.weight.penalty = TRUE
    )$predictions
    rmse_llf_ridge[sim] <- sqrt(mean((pred_llf_ridge - Y.test)^2)
    )
  }
  results <- rbind(results, data.frame(
    d = d,
    Method = "RF",         RMSE = mean(rmse_rf)
  ))
  results <- rbind(results, data.frame(
    d = d,
    Method = "LLF_CART",   RMSE = mean(rmse_llf_cart)
  ))
  results <- rbind(results, data.frame(
    d = d,
    Method = "LLF_Residual", RMSE = mean(rmse_llf_ridge)
  ))
}


ggplot(results, aes(x = d, y = RMSE, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_bw(base_size = 13) +
  labs(title = "Root Mean Squared Error by Model and Dimension",
       x = "Dimension (d)", y = "RMSE") +
  scale_color_manual(values = c("RF" = "green", "LLF_CART" = "darkorange", "LLF_Residual" = "blue"))