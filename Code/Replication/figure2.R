# create figure 2 
data <- read.csv("C:/Users/abdul/Downloads/Thesis(replicate code)/code/code/data/cps2018.csv")

data$agesq <- data$age^2
data$educsq <- data$educ^2

covariates <- c("agesq","educsq","uhrswork1","famsize","occ2010","occ10ly",
                "sex","race","marst","labforce","ind1950","classwkr","wkstat","metro","age","educ")
continuous.covariates <- which(covariates %in% c("agesq","educsq","uhrswork1","famsize"))
outcome <- "incwage"

data <- data[, c(covariates, outcome)]
data <- data[complete.cases(data), ]
data$incwage <- log(data$incwage + 1)

set.seed(1)
ntrain <- 10000
train_ix <- sample(nrow(data), ntrain)
train <- data[train_ix, ]
test <- data[-train_ix, ]
sub <- subset(test, famsize >= 6)

X <- model.matrix(~ . - incwage, data = train)[, -1]
Y <- train$incwage
X.test <- model.matrix(~ . - incwage, data = sub)[, -1]
truth <- sub$incwage

# OLS
ols.fit <- lm(incwage ~ ., data = train)
ols.preds <- predict(ols.fit, newdata = sub)

# Lasso met interacties
formula_lasso <- as.formula(paste("incwage ~ (", paste(covariates, collapse=" + "), ")^2"))
Xtrain_lasso <- model.matrix(formula_lasso, data = train)[, -1]
Xtest_lasso  <- model.matrix(formula_lasso, data = sub)[, -1]
lasso_fit <- cv.glmnet(Xtrain_lasso, Y, alpha=1, nfolds=5)
lasso_pred <- predict(lasso_fit, Xtest_lasso, s = "lambda.min")

# RF
rf_fit <- regression_forest(as.matrix(X), Y, honesty = TRUE)
rf_pred <- predict(rf_fit, as.matrix(X.test))$predictions

# LLF
llforest <- ll_regression_forest(as.matrix(X), Y, honesty=TRUE, enable.ll.split=TRUE, ll.split.weight.penalty=TRUE, tune.parameters="none")
lasso.mod <- cv.glmnet(as.matrix(X[, continuous.covariates]), Y, alpha=1)
lasso.coef <- coef(lasso.mod, s="lambda.min")
selected <- which(lasso.coef != 0)[-1]
if(length(selected)==0) selected <- continuous.covariates
llf.preds <- predict(llforest, as.matrix(X.test),
                     linear.correction.variables = selected,
                     ll.lambda = ll.lambda,
                     ll.weight.penalty = TRUE)$predictions

df <- data.frame(
  obs = truth,
  OLS = as.numeric(ols.preds),
  Lasso = as.numeric(lasso_pred),
  RF = as.numeric(rf_pred),
  LLF = as.numeric(llf.preds)
)
df_long <- melt(df, id.vars = "obs", variable.name = "model", value.name = "pred")

get_spline <- function(obs, pred) {
  splinefit <- smooth.spline(pred, obs, df = 5)
  data.frame(pred = splinefit$x, spline = splinefit$y)
}
splines_list <- lapply(split(df_long, df_long$model), function(d) get_spline(d$obs, d$pred))
splines_df <- do.call(rbind, Map(cbind, model = names(splines_list), splines_list))

ggplot(df_long, aes(x = pred, y = obs)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_line(data = splines_df, aes(x = pred, y = spline, color = model), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~model, scales = "free") +
  theme_bw() +
  labs(title = "Observed vs Predicted log(wages) for Family size ≥ 6",
       x = "Predicted log(wage)",
       y = "Observed log(wage)")

