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
predict(lm.fit, newdata=data.frame(lstat=c(5,10,15)),interval="prediction")
#recall that predicative intervals are going to wider than the confidence intervals


plot(boston$lstat,boston$medv)
#visually, we suspect some non-linearity.  We will addrss a bit later. 

#plot line over scatterplott
abline(lm.fit,lwd=3,col="red")

par(mfrow=c(2,2))

plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

#compute leverage for points
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##Working with multiple linear regression

