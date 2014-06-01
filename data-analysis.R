######## PSTAT 126 Project #######
##################################
## Multiple Regression Analysis ##
##################################

# Install class package
install.packages("class")
install.packages("ISLR")

library(class)
library(ISLR)

# Import the auto mpg data set
data(Auto)
head(Auto)

#######################################################
# Start Multiple Regression Analysis

#Analyze data with graphs
hist(Auto$mpg)
hist(Auto$cylinders)
hist(Auto$displacement)
hist(Auto$horsepower)
hist(Auto$weight)
hist(Auto$acceleration)
hist(Auto$year)
# Decide to use mileage as predictor variable

#Regression Analysis
#predicting the mileage from horsepower, weight, and acceleration
fit1<-lm(Auto$mpg~Auto$horsepower+Auto$weight+Auto$acceleration) 

summary(fit1)
# R^2 = 0.7064
# Regression Equation: Y = 45.68 - 0.0475*X1 - 0.0058*X2 - 0.0021*X3

#Plot Residuals
par(mfrow=c(1,1))
boxplot(fit1$residuals, main= "Box Plot of Residuals")

#Scatterplot Matrix
pairs(~Auto$horsepower+Auto$weight+Auto$acceleration,data=Auto, main="Scatterplot matrix")
#strong relationship between horsepower and weight

#######################################################
#plot residuals
par(mfrow=c(2,3))
plot(fit1$fitted, fit1$residuals)
abline(h=0)
qqnorm(fit1$residuals)
qqline(fit1$residuals)

plot(Auto$horsepower, fit1$residuals)
abline(h=0)
plot(Auto$weight, fit1$residuals)
abline(h=0)
plot(Auto$acceleration, fit1$residuals)
abline(h=0)

#######################################################
#Anova Test
anova(fit1)

#Analysis of Variance Table
#
#Response: Auto$mpg
#Df  Sum Sq Mean Sq  F value Pr(>F)    
#Auto$horsepower     1 14433.1 14433.1 800.7095 <2e-16 ***
#  Auto$weight         1  2392.1  2392.1 132.7058 <2e-16 ***
#  Auto$acceleration   1     0.0     0.0   0.0003 0.9866    
#Residuals         388  6993.8    18.0                    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#######################################################
#Confidence Intervals
confint(fit1)
#(Intercept)       (40.942863832,50.413721922)
#Auto$horsepower   (-0.078931810 -0.016059421)
#Auto$weight       (-0.006924953 -0.004653852)
#Auto$acceleration (-0.244559641  0.240428325)

