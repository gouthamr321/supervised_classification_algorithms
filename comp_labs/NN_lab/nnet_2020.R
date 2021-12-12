#####################################################################
## This code demonstrates Neural Networks in R                     ## 
##                                                                 ##
## Rachael Blair                                                   ##
## Created: December, 2020                                         ##
#####################################################################
rm(list = ls())
graphics.off()

library(neuralnet)
library(nnet)
ls("package:neuralnet")

# load the data
data(infert)
?infert
dim(infert)
head(infert)

# check for missing data and balance in response variable
which(is.na(infert) == TRUE) # no missing data # this is a check to see if there is any missing data
table(infert$case) # show  

# divide the data into test and training
set.seed(123)
indis <- sample(1:length(infert[,1]), 2/3*length(infert[,1]), replace = FALSE)
train <- infert[indis, ]
test <- infert[-indis, ]

# train a neural network
nn0 <- neuralnet(case ~ age + parity + induced + spontaneous, data = train, hidden = 1, err.fct = "ce", linear.output = FALSE) # hideen is the number of nodes in hidden layer, err function is cross entropy, linear output =False(classification)
quartz()
plot(nn0)

# controlling the neurons
nn1 <- neuralnet(case ~ age + parity + induced + spontaneous, data = train, hidden = 2, err.fct = "ce", linear.output = FALSE)
quartz()
plot(nn1)

# controlling the neurons
nn2 <- neuralnet(case ~ age + parity + induced + spontaneous, data = train, hidden = c(3, 2), stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
quartz()
plot(nn2)

############################################
## make prediction with the neural network
############################################
?predict.nn
pred <- predict(nn1, newdata = train)
y_hat_train <- round(pred)
train_err <- length(which(train$case != y_hat_train))/length(y_hat_train)
train_err

pred <- predict(nn1, newdata = test)
y_hat_test <- round(pred)
test_err <- length(which(test$case != y_hat_test))/length(y_hat_test)
test_err

###########################################
## put in a loop for tuning.
###########################################
train_err_store <- c()
test_err_store <- c()
for (i in 1:4){
	
	# fit neural network with "i" neurons --- i is the number of neurons in the hidden layer
	nn1 <- neuralnet(case ~ age + parity + induced + spontaneous, data = train, 
	hidden = i, stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
	
	# calculate the train error
	pred <- predict(nn1, newdata = train)
	y_hat_train <- round(pred)
	train_err <- length(which(train$case != y_hat_train))/length(y_hat_train)
	train_err_store <- c(train_err_store, train_err) #store the error at each iteration

	pred <- predict(nn1, newdata = test)
	y_hat_test <- round(pred)
	test_err <- length(which(test$case != y_hat_test))/length(y_hat_test)
	test_err_store <- c(test_err_store, test_err) #store the error at each iteration	
}
train_err_store
test_err_store # 2 neurons is optimal with train = .21, and TEST = .23 (min)

######################################
# Lets compare to logistic regression
######################################
fit <- glm(case ~ age + parity + induced + spontaneous, data = train, family = binomial)

# compute the test and training error
pred_train <- predict(fit, newdata = train, type = "response")
y_hat_train <- round(pred_train)
train_err <- length(which(train$case != y_hat_train))/length(y_hat_train)

pred_test <- predict(fit, newdata = test, type = "response")
y_hat_test <- round(pred_test)
test_err <- length(which(test$case != y_hat_test))/length(y_hat_test)

# LR results
train_err
test_err













