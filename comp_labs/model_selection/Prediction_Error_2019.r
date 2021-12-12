#######################################################################################
##
## This code is performs in sample methods, resampling ## methods for estimation of error and model 
## selection.
## Author: Rachael Hageman Blair
## Created: October 11, 2014
## Edited: July 20, 2019
##
##############################################################################################################

rm(list = ls())
library(ISLR)
library(bootstrap)
library(boot)  #install.packages("bootstrap")
library(leaps)

###########################
# load the data
###########################
data(Auto)
auto <- Auto #392 x 9

#############################################
#
# The hold out method
#
#############################################
set.seed(1)
train = sample(1:nrow(auto), .80*nrow(auto))
Y.train = auto$mpg[train]
Y.test = auto$mpg[-train]
X.train = auto[train, 4:5]
X.test = auto[-train, 4:5]

fit <- lm(mpg ~ horsepower + weight, data = auto[train,])
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error
train.error
##############
#############################################
# Utilize the hold-method over a spectrum
# of complexity paramters
#
#############################################
set.seed(1)
train = sample(1:nrow(auto), .80*nrow(auto))
Y.train = auto$mpg[train]
Y.test = auto$mpg[-train]

training = auto[train, 1:7]
testing = auto[-train, 1:7]

fit <- regsubsets(mpg~., data = training, method = 
"exhaustive", nvmax = 6)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp # another metrix
my_summary$bic # BIC

which.min(my_summary$cp) #Cp says 2 variables is best
which.min(my_summary$bic) #BIC says 2 variables is best

# Do the selection based on the "hold out method"
select = summary(fit)$outmat # here the rows are the number of varibales and columns are variables (each * denotes the )
train.error.store <- c()
test.error.store <- c()

# 
for (i in 1:6){
	temp <- which(select[i,] == "*")  # gets variable(s) that is best
	temp <- temp + 1 # because responce is included in training in first column
	
	red.training <- training[, c(1,temp)] # keep responce and the variables
	red.testing <- testing[,c(1,temp)]
	
	red.fit <- lm(mpg~., data = red.training)
	
	pred.train = predict(red.fit, newdata = red.training)
	pred.test = predict(red.fit, newdata = red.testing)
	
	test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
	train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
	
	train.error.store <- c(train.error.store, train.error)
	test.error.store <- c(test.error.store, test.error)

}

### Plot the results
upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)


quartz()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

#############################
# minute 35 of tutorial
## Calculate bootstrap predition error 
## for the best models of size "k"
###########################################
?boot #more generic function
?bootpred

# create functions that feed into "bootpred"
beta.fit <- function(X,Y){
	lsfit(X,Y)	
}

beta.predict <- function(fit, X){
	cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
	(Y-Yhat)^2
}

# Create X and Y
X <- auto[,2:7]
Y <- auto[,1]

# Practice, WLOG lets look at a single model
select = summary(fit)$outmat
temp <- which(select[3,] == "*") # best 3 variable model

res1 <- bootpred(X[,temp], Y, nboot = 500, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
res1[1] # app.err -- mean value of err.meas 
res1[2] # optim bootsteap estimate of optimism in app.err
res1[3] # err.632 the .632 bootstrap estimate of prediction error

# Generalize it, and search over the best possible subsets of size "k"
error_store <- c()
for (i in 1:6){
	# Pull out the model 
	temp <- which(select[i,] == "*")
	res <- bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
	error_store <- c(error_store, res[[3]]) # third is .632 error
}

quartz()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
lines(error_store, type = "o", lty = 3, col = "green")
legend("topright", c("training", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))

























