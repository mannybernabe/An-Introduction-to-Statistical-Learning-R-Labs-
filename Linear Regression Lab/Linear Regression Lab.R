remove(list=ls())

library(MASS);library(ISLR)
options(digits = 4)


#Load data sets in MASS package

data(Boston)
names(Boston)

boston<-Boston
remove(Boston)


lm.fit<-lm(medv~lstat, data=boston)

lm.fit
summary(lm.fit)
#F test shows significant.  At least one predictor is likely significant.
#Linear model explains ~54.4% of varation and the both 
#b0 and b1 are significant. 


names(lm.fit)
coef(lm.fit)

#obtain confidence intervals on coefficients
confint(lm.fit)

#prediction on new data, lstat=5,10,15.  With confidence intervals.
predict(lm.fit, newdata=data.frame(lstat=c(5,10,15)),interval="confidence")


#with predictive intervals.
predict(lm.fit, newdata=data.frame(lstat=c(5,10,15)),interval="prediction")
#recall that predicative intervals are always going to be 
# wider than the confidence intervals


plot(boston$lstat,boston$medv)
#visually, we suspect some non-linearity.  We will addrss a bit later. 

#plot line over scatterplott
abline(lm.fit,lwd=3,col="red")

par(mfrow=c(2,2))

plot(lm.fit)

par(mfrow=c(1,1))
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
#residuals show evidence of non-linearity.




#compute leverage for points
plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))
#point 375 has the most leverage on the regression. 


##Working with multiple linear regression
##

lm.fit<-lm(medv~lstat+age,data=boston)
summary(lm.fit)

#regression on all valiable variables
lm.fit<-lm(medv~.,data=boston)
summary(lm.fit)

names(summary(lm.fit))

summary(lm.fit)$r.sq
summary(lm.fit)$sigma # re

#mean squared error
mean(summary(lm.fit)$residuals^2)

#root mean squared error (less used )
sqrt(mean(summary(lm.fit)$residuals^2))

#RSS
sum(summary(lm.fit)$residuals^2)

#compute variance inflation for each factor
library(car)
vif(lm.fit)

#regression on all but one factor
lm.fit1<-lm(medv~.-age,data=boston)
summary(lm.fit1)

#alternatively the update() function:
lm.fit1<-update(lm.fit,~.-age)


###
###Interaction Effects
###
#"The syntax lstat:black tells R 
# to include an interaction term between lstat and black. 
# The syntax lstat*age simultaneously includes lstat, age, 
# and the interaction term lstat×age as predictors; 
# it is a shorthand for lstat+age+lstat:age"
