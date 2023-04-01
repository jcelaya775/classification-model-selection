rm(list=ls())
library(ISLR)
set.seed(1)

# a) Split data into training and test sets
len = nrow(College) / 2
train = sample(1:nrow(College), len)
test = -train
College.train = College[train,]
College.test = College[test,]

# b) Fit linear model and test
lm.fit = lm(Apps ~ ., data = College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test$Apps - lm.pred)^2)

# c) Fit a ridge regression model and test
library(glmnet)
train.mat = model.matrix(Apps ~ ., data = College.train)
test.mat = model.matrix(Apps ~ ., data = College.test)
ridge.mod = cv.glmnet(train.mat, College.train$Apps, alpha = 0)
best.lambda = ridge.mod$lambda.min
ridge.mod
ridge.pred = predict(ridge.mod, newx = test.mat, s = best.lambda)
mean((College.test$Apps - ridge.pred)^2)

# d) Fit a lasso model and test
lasso.mod = cv.glmnet(train.mat, College.train$Apps, alpha = 1)
best.lambda = lasso.mod$lambda.min
lasso.mod
lasso.pred = predict(lasso.mod, newx = test.mat, s = best.lambda)
mean((College.test$Apps - lasso.pred)^2)

# e) Fit a PCR model and test
library(pls)
pcr.fit = pcr(Apps ~ ., data = College.train, scale = T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP", main = "PCR")
pcr.pred = predict(pcr.fit, College.test, ncomp = 10)
mean((College.test$Apps - pcr.pred)^2)

# f) Fit a PLS model and test
pls.fit = plsr(Apps ~ ., data = College.train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP", main = "PLS")
pls.pred = predict(pls.fit, College.test, ncomp=6)
mean((pls.pred - College.test$Apps)^2)
