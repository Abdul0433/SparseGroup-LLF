# load the data
if (!dir.exists("results")) dir.create("results")

library(grf)
library(glmnet)
library(ggplot2)
library(BART)
library(xgboost)
library(rlearner)
#library(hte)
library(dplyr)
library(splines)

# Load data
data <- read.csv("C:/Users/abdul/Downloads/Thesis(replicate code)/code/code/data/cps2018.csv")

# Add age^2 and educ^2
data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq", "educsq", "uhrswork1", "famsize", "occ2010", "occ10ly", "sex", "race",
                "marst", "labforce", "ind1950", "classwkr", "wkstat", "metro")
continuous.covariates <- which(covariates %in% c("agesq", "educsq", "uhrswork1", "famsize"))
outcome <- "incwage"

data <- data[, c(covariates, outcome)]
data <- data[complete.cases(data), ]

# Transform outcome
data$incwage <- log(data$incwage + 1)

# Setup
size.test <- 40000
sample.sizes <- c(2000, 5000, 10000, 50000)
if (efficient.run) {
  sample.sizes <- c(2000, 5000, 10000)
}

ptm <- proc.time()

mse.sample.sizes <- data.frame(t(sapply(sample.sizes, function(size) {
  index.train <- sample(1:nrow(data), size = size, replace = FALSE)
  
  X <- data[index.train, covariates]
  Y <- data$incwage[index.train]
  
  complete_rows <- complete.cases(X, Y)
  X <- X[complete_rows, ]
  Y <- Y[complete_rows]
  
  results <- data.frame(t(sapply(1:num.reps, function(i) {
    print(i)
    index.test <- sample((1:nrow(data))[-index.train], size = size.test, replace = FALSE)
    
    X.test <- data[index.test, covariates]
    truth <- data$incwage[index.test]
    
    complete_test_rows <- complete.cases(X.test, truth)
    X.test <- X.test[complete_test_rows, ]
    truth <- truth[complete_test_rows]
    
    llforest <- ll_regression_forest(as.matrix(X), Y, honesty = TRUE, enable.ll.split = TRUE,ll.split.weight.penalty = TRUE, tune.parameters = "none")
    
    # Lasso for linear correction
    lasso.mod <- cv.glmnet(as.matrix(X[, continuous.covariates]), Y, alpha = 1)
    lasso.coef <- coef(lasso.mod, s = "lambda.min")
    selected <- which(lasso.coef != 0)[-1]  # drop intercept
    if (length(selected) == 0) {
      selected <- continuous.covariates
    }
    
    llf.preds <- predict(llforest, as.matrix(X.test),
                         linear.correction.variables = selected,
                         ll.lambda = ll.lambda,
                         ll.weight.penalty = TRUE)$predictions
    llf.mse <- mean((llf.preds - truth)^2)
    
    
    forest <- regression_forest(as.matrix(X), Y, honesty = TRUE, tune.parameters = tune)
    rf.preds <- predict(forest, as.matrix(X.test))$predictions
    rf.mse <- mean((rf.preds - truth)^2)
    
    ols.form <- as.formula(paste("Y", paste(covariates, collapse = "+"), sep = "~"))
    dd.ols <- cbind(Y, X)
    ols.fit <- lm(ols.form, dd.ols)
    ols.preds <- predict(ols.fit, X.test)
    ols.mse <- mean((ols.preds - truth)^2)
    
    mm <- model.matrix(~.^2, data = X)
    lasso.mod <- cv.glmnet(mm, Y, alpha = 1)
    mmtest <- model.matrix(~.^2, data = X.test)
    lasso.preds <- predict(lasso.mod, newx = mmtest, lambda = lasso.mod$lambda.min)
    lasso.mse <- mean((lasso.preds - truth)^2)
    
    bart.mod <- wbart(X, Y, X.test, ndpost = ndpost)
    bart.preds <- bart.mod$yhat.test.mean
    bart.mse <- mean((bart.preds - truth)^2)
    
    boost.cv.fit <- rlearner::cvboost(as.matrix(X), Y, ntrees_max = xgb_max, num_search_rounds = num_search_rounds)
    xgb.preds <- predict(boost.cv.fit, as.matrix(X.test))
    xg.mse <- mean((xgb.preds - truth)^2)
    
    return(c(ols.mse, llf.mse, rf.mse, lasso.mse, xg.mse, bart.mse))
  })))
  
  mses <- colMeans(results)
  sds <- apply(results, 2, sd)
  as.numeric(c(mses, sds))
})))

colnames(mse.sample.sizes) <- c("OLS", "LLF", "RF", "Lasso", "XG", "BART",
                                "OLS.sd", "LLF.sd", "RF.sd", "Lasso.sd", "XG.sd", "BART.sd")
mse.sample.sizes$size <- sample.sizes

cat(paste("One run took", (1 / 60) * round(proc.time() - ptm)[3]), "minutes")

write.csv(mse.sample.sizes, "results/wages_sample_sizestest.csv", row.names = FALSE)
