rm(list=ls())
library(MASS)
set.seed(1)


# Best subset selection
library(leaps)
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

# Use 10-fold CV to choose best subset model with lowest RMSE
k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k), nrow(Boston), replace = T)
cv.errors = matrix(NA, k, p, dimnames = list(NULL, paste(1:p)))
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i,],
                        nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i,], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
cv.rmse = sqrt(mean.cv.errors)
min.rmse = which.min(cv.rmse)
plot(cv.rmse, type = "b", xlab = "# of Predictors", ylab = "RMSE",
     main = "Best Subset Selection", pch = 16)
cv.rmse[min.rmse]


# Ridge regression
library(glmnet)
x = model.matrix(crim ~ ., Boston)[, -1]
y = Boston$crim
train = sample(1:nrow(Boston), nrow(Boston) / 2)
test = -train
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out, main = "Ridge Regression")
best.lambda = cv.out$lambda.min
best.lambda
ridge.pred = predict(cv.out, s = best.lambda, newx = x[test,])
sqrt(mean((ridge.pred - y[test])^2)) # RMSE


# Lasso
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out, main = "Lasso")
best.lambda = cv.out$lambda.min
ridge.pred = predict(cv.out, s = best.lambda, newx = x[test,])
sqrt(mean((ridge.pred - y[test])^2)) # RMSE


# PCR
library(pls)
pcr.fit = pcr(crim ~ ., data = Boston, validation = "CV")
summary(pcr.fit)


# PLS
pls.fit = plsr(crim ~ ., data = Boston, validation = "CV")
summary(pls.fit)
