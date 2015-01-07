remove(list=ls())

library(MASS);library(ISLR)
options(digits = 4)


#Load data sets in MASS package
data()

data(Boston)
names(Boston)

boston<-Boston
remove(Boston)


lm.fit<-lm(medv~lstat, data=boston)

names(lm.fit)

coef(lm.fit)

#obtain confidence intervals on coefficients
confint(lm.fit)

#prediction on new data, lstat=5,10,15.  With confidence intervals.

predict(lm.fit, newdata=data.frame(lstat=c(5,10,15)),interval="confidence")


#with predictive intervals.
predict(lm.fit, newdata=data.frame(lstat=c(5,10,15)),interval="confidence")