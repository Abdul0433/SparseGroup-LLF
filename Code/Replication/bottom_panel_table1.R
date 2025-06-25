

if (!dir.exists("results")) dir.create("results")

source("efficiency_controls.R")



raw <- read.csv("C:/Users/Gebruiker/Downloads/Thesis(replicate code) (2)/Thesis(replicate code)/code/code/data/cps2018.csv")
vars_needed <- c("age","educ","uhrswork1","famsize","occ2010","occ10ly",
                 "sex","race","marst","labforce","ind1950","classwkr",
                 "wkstat","metro","incwage")
raw <- raw[complete.cases(raw[, vars_needed]), ]
# Transform outcome and add derived covariates
raw$incwage <- log(raw$incwage + 1)
raw$agesq   <- raw$age^2
raw$educsq  <- raw$educ^2

covariates <- c("agesq","educsq","uhrswork1","famsize","occ2010","occ10ly",
                "sex","race","marst","labforce","ind1950","classwkr",
                "wkstat","metro")
cont_cov   <- which(covariates %in% c("agesq","educsq","uhrswork1","famsize"))


ntrain   <- 20000
size_test <- 40000
num_reps <-  100

age_q <- quantile(raw$age, c(0.05, 0.95))
race_counts <- table(raw$race)
race_counts_sorted <- sort(race_counts)
cum_counts <- cumsum(race_counts_sorted)
less_sampled <- names(race_counts_sorted)[cum_counts <= 8750]
cat("Bottom‐quartile race codes:", less_sampled, 
    "→ total obs =", sum(race_counts[less_sampled]), "\n")

results <- list(
  extreme   = matrix(NA, num_reps, 7, dimnames=list(NULL, c("OLS","Lasso","XGB","BART","RF","LLF","ntest"))),
  less_race = matrix(NA, num_reps, 7, dimnames=list(NULL, c("OLS","Lasso","XGB","BART","RF","LLF","ntest"))),
  fam6      = matrix(NA, num_reps, 7, dimnames=list(NULL, c("OLS","Lasso","XGB","BART","RF","LLF","ntest")))
)

for (i in seq_len(num_reps)) {
  cat("Replication", i, "of", num_reps, "\n")
  train_idx <- sample(nrow(raw), ntrain)
  remaining <- setdiff(seq_len(nrow(raw)), train_idx)
  test_idx  <- sample(remaining, size_test)
  
  train <- raw[train_idx, ]
  test  <- raw[test_idx, ]
  
  X_train <- train[, covariates]; Y_train <- train$incwage
  X_test  <- test[,  covariates]; Y_test  <- test$incwage
  
  #Local Linear Forest
  llf_forest <- ll_regression_forest(as.matrix(X_train), Y_train,
                                     honesty=TRUE, enable.ll.split = TRUE, tune.parameters="none")
  cv_lasso   <- cv.glmnet(as.matrix(X_train[, cont_cov]), Y_train, alpha=1)
  sel_vars   <- which(coef(cv_lasso, s="lambda.min") != 0)[-1]
  if (length(sel_vars)==0) sel_vars <- cont_cov
  pred_llf   <- predict(llf_forest, as.matrix(X_test),
                        linear.correction.variables=sel_vars,
                        ll.lambda=ll.lambda,
                        ll.weight.penalty=TRUE)$predictions
  
  #Random Forest
  rf_forest <- regression_forest(as.matrix(X_train), Y_train,
                                 honesty=TRUE, tune.parameters=tune)
  pred_rf   <- predict(rf_forest, as.matrix(X_test))$predictions
  
  #OLS
  df_ols <- cbind(Y_train, X_train)
  formula_ols <- as.formula(paste("Y_train ~", paste(covariates, collapse="+")))
  fit_ols <- lm(formula_ols, data=df_ols)
  pred_ols <- predict(fit_ols, newdata=test)
  
  #Lasso 
  mm_train <- model.matrix(~.^2, data=X_train)
  mm_test  <- model.matrix(~.^2, data=X_test)
  l2_lasso <- cv.glmnet(mm_train, Y_train, alpha=1)
  pred_lasso <- predict(l2_lasso, newx=mm_test, s="lambda.min")
  
  #BART
  bart_mod   <- wbart(X_train, Y_train, X_test, ndpost=ndpost)
  pred_bart  <- bart_mod$yhat.test.mean
  
  #XGBoost
  xgb_mod    <- rlearner::cvboost(as.matrix(X_train), Y_train,
                                  ntrees_max=xgb_max,
                                  num_search_rounds=num_search_rounds)
  pred_xgb   <- predict(xgb_mod, as.matrix(X_test))
  
  idx_extreme <- which(test$age <= age_q[1] | test$age >= age_q[2])
  idx_race    <- which(test$race %in% less_sampled)
  idx_fam6    <- which(test$famsize >  6)
  
  # Compute MSEs and ntest
  subset_list <- list(extreme=idx_extreme, less_race=idx_race, fam6=idx_fam6)
  for (nm in names(subset_list)) {
    idx <- subset_list[[nm]]
    mse_vals <- c(
      OLS   = mean((pred_ols[idx]    - Y_test[idx])^2),
      Lasso = mean((pred_lasso[idx] - Y_test[idx])^2),
      XGB   = mean((pred_xgb[idx]   - Y_test[idx])^2),
      BART  = mean((pred_bart[idx]  - Y_test[idx])^2),
      RF    = mean((pred_rf[idx]    - Y_test[idx])^2),
      LLF   = mean((pred_llf[idx]   - Y_test[idx])^2)
    )
    results[[nm]][i, ] <- c(mse_vals, length(idx))
  }
}

final <- lapply(results, function(mat) {
  mses <- colMeans(mat[,1:6])
  sds  <- apply(mat[,1:6], 2, sd)
  n_avg <- mean(mat[,7])
  c(Avg.ntest = round(n_avg),
    round(mses,2),
    paste0("(", round(sds,2), ")"))
})

out <- do.call(rbind, final)
colnames(out) <- c("Avg.n","OLS","Lasso","XGB","BART","RF","LLF","OLS.sd","Lasso.sd","XGB.sd","BART.sd","RF.sd","LLF.sd")
rownames(out) <- c("Extreme ages","Less sampled races","Family size >= 6")

write.csv(out, "results/bottom_panel_table1.csv", row.names=TRUE)
print(out)
