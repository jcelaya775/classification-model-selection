setwd("c:/repos/r/project-2")

if (!require("MASS")) install.packages("MASS")
if (!require("class")) install.packages("class")

?read.csv
read.auto.data = function() {
  Auto = read.csv("Auto.csv", na.strings = c("", " ", "NA"))
  Auto$horsepower = as.numeric(Auto$horsepower) # convert horsepower to numeric
  na.omit(Auto)
  return(Auto)
}

# Read data
Auto = read.auto.data()
Auto = data.frame(Auto)
attach(Auto)
fix(Auto)
summary(Auto)

# a): Add binary variable mpg01 to data set
mpg01 = rep(0, length(mpg)) # mpg01 = 0 if mpg < median(mpg)
mpg01[mpg > median(mpg)] = 1 # else mpg01 = 1 if mpg > median(mpg)
Auto$mpg01 = mpg01
table(Auto$mpg01)
Auto[mpg01 == 0,]

# b): Exploring the data graphically
Auto = subset(Auto, select = -c(name)) # extract only numerical data
pairs(Auto)

# Box plots for each pair of variables
par(mfrow = c(3, 3))
for (col in colnames(Auto[,-c(1, 9)])) {
 boxplot(Auto$mpg01, Auto[,col], xlab = col, ylab = "MPG > Median") 
}

# c): Randomly split data into a training and test sets
train = sample(c(TRUE, FALSE), nrow(Auto), replace=TRUE)
Auto.train = Auto[train,]
Auto.test = Auto[!train,]

test.model = function(model, test.data) {
  model.pred = predict(model, test.data) 
  # 
}

# d) Perform and test LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + displacement + horsepower, data = Auto, subset = train)
lda.fit
plot(lda.fit)
?predict
lda.pred = predict(lda.fit, Auto.test)
lda.pred = predict(lda.fit, data = Auto, subset = test)
Auto$mpg01
lda.pred


# e) Perform and test QDA
qda.fit = qda(mpg01 ~ cylinders + displacement + horsepower, data = Auto, subset = train)
qda.fit

# f) Perform and test logistic regression
glm.fits = glm(mpg01 ~ cylinders + displacement + horsepower, data = Auto, subset = train)
glm.fits

# f) Perform and test logistic regression
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)