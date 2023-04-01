rm(list=ls())

# a) Generate a simulated data set
set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

# b) Scatterplot
plot(x, y)

# c) Find LOOCV errors from models
library(boot)
data.set = data.frame(x, y)
glm.fit = glm(y ~ x)
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 2))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 3))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 4))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

# d) Repeat with a different seed
set.seed(2)
glm.fit = glm(y ~ x)
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 2))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 3))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

glm.fit = glm(y ~ poly(x, 4))
cv.err = cv.glm(data.set, glm.fit)
cv.err$delta

summary(glm.fit)
