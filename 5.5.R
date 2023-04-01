rm(list=ls())
set.seed(1)
library(ISLR)
?Default

# a) Fit logistic regression model
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
glm.fit

# b) Estimate test error with validation approach
train = sample(1:nrow(Default), nrow(Default) / 2)
Default.test = Default[-train, ]
glm.fit = glm(default ~ income + balance, data = Default, family = binomial,
              subset = train)
glm.pred = rep("No", nrow(Default) / 2)
glm.probs = predict(glm.fit, Default.test, type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default.test$default)

# d) Include student dummy variable
train = sample(1:nrow(Default), nrow(Default) / 2)
Default.test = Default[-train, ]
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial,
              subset = train)
glm.pred = rep("No", nrow(Default) / 2)
glm.probs = predict(glm.fit, Default.test, type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default.test$default)
