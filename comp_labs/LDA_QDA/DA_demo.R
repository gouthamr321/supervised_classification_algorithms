#########################
#
# This code performs LDA, QDA and RDA
#
# Created: 7/2021
# Edited: 
#########################
rm(list = ls())

library(klaR) # install.packages
library(MASS) # install.packages

setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/LDA_QDA")

# load data
load('pima.RData')
head(pima)
my_pima <- pima[ ,-8]
head(my_pima)

#cor(my_pima)

# Create a test and training set
set.seed(123)
indis <- sample(1:nrow(my_pima), round(2/3*nrow(my_pima)), replace = FALSE)

pima_train <- my_pima[indis, ]
pima_test <- my_pima[-indis, ]
dim(pima_train)
dim(pima_test)

####################################
# LDA
#
####################################
lda.fit <- lda(class~., data = pima_train)
lda.fit

quartz()
plot(lda.fit) # plot histogram for histogram of normal and diabetic

# make predictions for the test and training.
test_pred <- predict(lda.fit, newdata = pima_test)
class(test_pred)
data.frame(test_pred$class, test_pred$posterior, test_pred$x)[1:5,]

train_pred <- predict(lda.fit, newdata = pima_train)
train_pred$class

# compute the error rates
train_error <- (1/length(pima_train$class))*length(which(pima_train$class != train_pred$class))
test_error <- (1/length(pima_test$class))*length(which(pima_test$class != test_pred$class))
train_error
test_error

####################################
# QDA
#
####################################
?qda
qda.fit <- qda(class~., data = pima_train)
qda.fit

train_pred <- predict(qda.fit, newdata = pima_train)
test_pred <- predict(qda.fit, newdata = pima_test)

y_hat_train <- train_pred$class
y_hat_test <- test_pred$class

y_true_train <- pima_train$class
y_true_test <- pima_test$class

train_err <- (1/length(y_hat_train))*length(which(y_true_train != y_hat_train))
test_err <- (1/length(y_hat_test))*length(which(y_true_test != y_hat_test))

train_err
test_err # lda is doing a better job.

####################################
# RDA
#
####################################
lda_fit <- rda(class~., data = pima_train, regularization = c(gamma=0, lambda=1))   
qda_fit <- rda(class~., data = pima_train, regularization = c(gamma=0, lambda=0))
rda_fit <- rda(class~., data = pima_train, regularization = c(gamma=0, lambda=.5))

lda_fit$error.rate
qda_fit$error.rate
rda_fit$error.rate

y_hat_lda_test <- predict(lda_fit, newdata=pima_test)$class
y_hat_qda_test <- predict(qda_fit, newdata=pima_test)$class
y_hat_rda_test <- predict(rda_fit, newdata=pima_test)$class

?seq
alpha = seq(from = 0, to = 1, by = .1)
err_store <- c()
for (i in 1:length(alpha)){
	rda_fit <- rda(class~., data = pima_train, regularization = c(gamma=0, lambda=alpha[i]))
	y_hat_test <- predict(rda_fit, newdata=pima_test)$class
	err <- (1/length(y_hat_test))*length(which(y_hat_test != pima_test$class))
	err_store <- c(err_store, err)
}

quartz() # call a window
plot(err_store, type = "l", xlab = "alpha iteration")













