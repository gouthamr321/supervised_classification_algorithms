#############################
# LASSO AND RIDGE
#
# Rachael Hageman Blair
# Created: 9/24/14
# Editied: 9/2020
#############################

rm(list = ls())
#library(ElemStatLearn)
library(glmnet)  #install.packages("glmnet)

# alpha is the type of penalty alpha=0 is ridge and alpha =1 is lasso
#############################
##
## load the prostate data
##############################
setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/LASSO_RIDGE")
load("prostate.RData")

my_dats <- na.omit(prostate[,1:9]) 
X <- as.matrix(my_dats[,1:8])
Y <- my_dats[,9]

#############################
##
## Fit a Ridge Regression Model
##############################
ridge.mod = glmnet(X, Y, alpha=0)
names(ridge.mod)
coef(ridge.mod)
dim(coef(ridge.mod)) # This goes thru a 100 lambda values for each of the 9 variables --- selected through some  process

# Look at different lambdas
ridge.mod$lambda[10]
coef(ridge.mod)[,10]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,10]^2))
l2_norm

# Look at different lambdas
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,50]^2))
l2_norm

# Look at different lambdas
ridge.mod$lambda[100]
coef(ridge.mod)[,100]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,100]^2))
l2_norm

# Predict the model for a "new value" of lamba
?predict.glmnet
predict(ridge.mod, s = .0005, type = "coefficient")

predict(ridge.mod, s = .75, type = "coefficient")

###################################
#
# Model Selection 
###################################
?cv.glmnet
set.seed(12345)
train <- sample(1:nrow(X), round(nrow(X)/2))

cv.out <- cv.glmnet(X[train,], Y[train], alpha = 0)
plot(cv.out)

names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")

ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = X[-train,], type = "response")

y_hat <- ridge.pred2
y_true <- Y[-train]
test_error <- sum((y_hat - y_true)^2)  #test_error

######################################
#
#  The LASSO
######################################
lasso.mod <- glmnet(X[train,], Y[train], alpha = 1)
x11()
plot(lasso.mod)

# LEts look at coefficients....
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

lasso.mod$lambda[70]
coef(lasso.mod)[,70]

lasso.mod$lambda[50]
coef(lasso.mod)[,50]

lasso.mod$lambda[20]
coef(lasso.mod)[,20]

# Best Lambda
cv.out = cv.glmnet(X[train,], Y[train], alpha = 1)
bestlam = cv.out$lambda.min

lasso.pred <- predict(lasso.mod, s = bestlam, type = "coefficients")

lasso.pred2 <- predict(lasso.mod, s = bestlam, newx = X[-train,], type = "response")

y_hat_lasso <- lasso.pred2
y_true <- Y[-train]

test_error_lasso <- sum((y_hat_lasso-y_true)^2) #26.33054
