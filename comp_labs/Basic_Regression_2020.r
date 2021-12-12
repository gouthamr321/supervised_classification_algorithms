###########################################################
##
## This code demonstrates single and multiple regression
##
## Created: September 9, 2014
## Edited: September, 2020
##
###########################################################
rm(list = ls())
#library(ElemStatLearn) #dont need it
library(MASS)
#setwd("~/Desktop/STA_545_Fall2014")

############################
# Load the data
############################
data(Boston)
?Boston
names(Boston)

boston <- Boston
## WHat is R-squared value
############################
# Simple Regression
############################
fit <- lm(medv ~ lstat, data = boston) # here twiddle is equivelent = symbol
names(fit)
summary(fit)

# Compute confidence interval
confint(fit)

# Compute a Confidence interval for specific values of the 	predictors....
predict(fit, newdata = data.frame(lstat = c(5,10,15)))

predict(fit, newdata = data.frame(lstat = c(5,10,15)), interval = "confidence")

#############################################
## Multiple Regression Model
#############################################
# build a small model
small_model <- lm(medv ~ lstat + age, data = boston)
summary(small_model)

large_model <- lm(medv ~ ., data = boston) # dot includes all the varibales except for the predictor
summary(large_model)

trimmed_model <- lm(medv~ .-age, data = boston)
my_summary <- summary(trimmed_model)
names(my_summary)
my_summary$coefficients
my_summary$adj.r.squared




