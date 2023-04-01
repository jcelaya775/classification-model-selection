rm(list=ls())
# a) Generate simulated data
set.seed(1)
X = rnorm(100)
eps = rnorm(100)
summary(X)
summary(eps)

# b) Generate response vector Y
Y = 2 + 2*X + 0.5*X^2 + 4*X^3 + eps

# c) Perform best subset selection
library(leaps)
data.set = data.frame(x = X, y = Y)
regfit.full = regsubsets(y ~ poly(x, 10), data = data.set, nvmax = 10)
reg.summary = summary(regfit.full)

# Choose best models for C_p, BIC, and adjusted R^2
min.cp = which.min(reg.summary$cp)
min.bic = which.min(reg.summary$bic)
max.adjr2 = which.max(reg.summary$adjr2)

# Plot C_p, BIC, and adjusted R^2
plot(reg.summary$cp, type = "l", xlab = "# of Predictors", ylab = "Cp", main = "Best Subset Selection")
points(x = min.cp, y = reg.summary$cp[min.cp], col = "red", pch = 16)

plot(reg.summary$bic, type = "l", xlab = "# of Predictors", ylab = "BIC", main = "Best Subset Selection")
points(x = min.bic, y = reg.summary$bic[min.bic], col = "red", pch = 16)

plot(reg.summary$adjr2, type = "l", xlab = "# of Predictors", ylab = "Adjusted R^2", main = "Best Subset Selection")
points(x = max.adjr2, y = reg.summary$adjr2[max.adjr2], col = "red", pch = 16)

coefficients(regfit.full, id = 3)

# d) Perform forward stepwise selection
regfit.fwd = regsubsets(y ~ poly(x, 10), data = data.set, nvmax = 10, method = "forward")
reg.summary = summary(regfit.fwd)

# Choose best models for C_p, BIC, and adjusted R^2
min.cp = which.min(reg.summary$cp)
min.bic = which.min(reg.summary$bic)
max.adjr2 = which.max(reg.summary$adjr2)

# Plot C_p, BIC, and adjusted R^2
plot(reg.summary$cp, type = "l", xlab = "# of Predictors", ylab = "Cp", main = "Forward Stepwise Selection")
points(x = min.cp, y = reg.summary$cp[min.cp], col = "red", pch = 16)

plot(reg.summary$bic, type = "l", xlab = "# of Predictors", ylab = "BIC", main = "Forward Stepwise Selection")
points(x = min.bic, y = reg.summary$bic[min.bic], col = "red", pch = 16)

plot(reg.summary$adjr2, type = "l", xlab = "# of Predictors", ylab = "Adjusted R^2", main = "Forward Stepwise Selection")
points(x = max.adjr2, y = reg.summary$adjr2[max.adjr2], col = "red", pch = 16)

coefficients(regfit.fwd, id = 3)

# Perform backward stepwise selection
regfit.bwd = regsubsets(y ~ poly(x, 10), data = data.set, nvmax = 10, method = "backward")
reg.summary = summary(regfit.bwd)

# Choose best models for C_p, BIC, and adjusted R^2
min.cp = which.min(reg.summary$cp)
min.bic = which.min(reg.summary$bic)
max.adjr2 = which.max(reg.summary$adjr2)

# Plot C_p, BIC, and adjusted R^2
plot(reg.summary$cp, type = "l", xlab = "# of Predictors", ylab = "Cp", main = "Backward Stepwise Selection")
points(x = min.cp, y = reg.summary$cp[min.cp], col = "red", pch = 16)

plot(reg.summary$bic, type = "l", xlab = "# of Predictors", ylab = "BIC", main = "Backward Stepwise Selection")
points(x = min.bic, y = reg.summary$bic[min.bic], col = "red", pch = 16)

plot(reg.summary$adjr2, type = "l", xlab = "# of Predictors", ylab = "Adjusted R^2", main = "Backward Stepwise Selection")
points(x = max.adjr2, y = reg.summary$adjr2[max.adjr2], col = "red", pch = 16)

coefficients(regfit.bwd, id = 3)

# Fit a lasso model
library(glmnet)
x.mat =  model.matrix(y ~ poly(x, 10), data = data.set)[, -1]
lasso.mod = cv.glmnet(x.mat, Y, alpha = 1)
lasso.mod
best.lamda = lasso.mod$lambda.min
best.lamda

plot(lasso.mod, main = "Lasso")

# Fit best model to full data set
best.model = glmnet(x.mat, Y, alpha = 1)
predict(best.model, s = best.lamda, type = "coefficients")

# f) Generate new response vector
Y = 2 + 7*X^7 + eps

# Best subset selection
data.set = data.frame(x = X, y = Y)
regfit.full = regsubsets(y ~ poly(x, 10), data = data.set, nvmax = 10)
reg.summary = summary(regfit.full)

# Choose best models for C_p, BIC, and adjusted R^2
which.min(reg.summary$cp)
which.min(reg.summary$bic)
which.max(reg.summary$adjr2)

coefficients(regfit.full, id = 1)
coefficients(regfit.full, id = 2)
coefficients(regfit.full, id = 4)

# Lasso
x.mat = model.matrix(y ~ poly(x, 10), data = data.set)[, -1]
lasso.mod = cv.glmnet(x.mat, Y, alpha = 1)
best.lambda = lasso.mod$lambda.min
best.lambda

# Fit best model to full data set
best.model = glmnet(x.mat, Y, alpha = 1)
predict(best.model, s = best.lamda, type = "coefficients")
