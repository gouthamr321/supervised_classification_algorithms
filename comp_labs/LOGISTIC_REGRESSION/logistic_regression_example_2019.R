#########################################
# This code performs Logistic Regression
# Rachael Blair
#
# Created: October 3, 2014
# Edited: October 2020
#########################################

rm(list = ls())
#library(MMST)
library(caret) #install.packages("caret")

# Set the working directory
setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/LOGISTIC_REGRESSION")

load("pima.RData")
my_pima <- pima[,-8]

#########################################
# Create a training and test set
#########################################
set.seed(123)
train <- sample(1:nrow(my_pima), .66*nrow(my_pima))
pima_train <- my_pima[train,]
pima_test <- my_pima[-train,]

y_true_train <- as.numeric(pima_train$class)-1
y_true_test <- as.numeric(pima_test$class)-1

#########################################
# Logistic Regression
#########################################
glm.fit <- glm(class ~., data = pima_train, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = pima_train, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = pima_test, type = "response")
y_hat_test <- round(glm.probs.test)

#########################################
#  Calculate the error rates
########################################
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err

#########################################
#  Confusion Matrix
########################################
conf <- confusionMatrix(as.factor(y_hat_test), as.factor(y_true_test))
names(conf)
conf$table





