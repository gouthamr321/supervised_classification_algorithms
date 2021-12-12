# Homework 5 problem 3 solution
rm(list = ls())

library(ISLR2)
library(caret)
library(e1071)
library(MASS)

data(OJ)
dataset_OJ <- OJ


#(a) Create a training set containing a random sample of 800 observations, and a
#test set containing the remaining observations.
set.seed(1)
percent_training <- 800/nrow(dataset_OJ)
dataset_OJ$Purchase <- as.numeric(dataset_OJ[,1]) - 1
train <- sample(1:nrow(dataset_OJ), percent_training* nrow(dataset_OJ))
dataset_OJ_training = dataset_OJ[train,]
dataset_OJ_testing = dataset_OJ[-train,]

#(b) Fit a support vector classifier to the training data using cost = 0.01, with
#Purchase as the response and the other variables as predictors. Use the
# summary() function to produce summary statistics, and describe the results
# obtained.


# (c) What are the training and test error rates?

fit <- svm(Purchase~., data = dataset_OJ_training, kernel = "linear", cost = 0.01, scale = FALSE)

y_hat_training <- (predict(fit, newdata = dataset_OJ_training) > 0.5) * 1
y_true_training <- dataset_OJ_training$Purchase
cm_pred_training <- confusionMatrix(as.factor(y_hat_training),as.factor(y_true_training)) # misclass rate 0.1825
summary(fit)
y_hat_testing <- (predict(fit, newdata = dataset_OJ_testing) > 0.5) * 1
y_true_testing <- dataset_OJ_testing$Purchase
cm_pred_testing <- confusionMatrix(as.factor(y_hat_testing),as.factor(y_true_testing))

# (d) Use the tune() function to select an optimal cost. Consider values in the range
# 0.01 to 10.
tune.model.rad <- tune(svm, Purchase ~., data = dataset_OJ_training, kernel = "linear", ranges = list(cost = c(.001, .01, .1, 1, 3, 5, 7, 10)))
summary(tune.model.rad)
names(tune.model.rad)
bestmod <- tune.model.rad$best.model
# optimal cost to be 0.01


# (e) Compute the training and test error rates using this new value for cost.

y_hat_train_optimized <- (predict(bestmod, newdata = dataset_OJ_training) > 0.5) * 1
y_hat_test_optimized <- (predict(bestmod, newdata = dataset_OJ_testing) > 0.5) * 1
cm_pred_training_opt <- confusionMatrix(as.factor(y_hat_train_optimized), as.factor(y_true_training)) # misclassification rate = 0.1762
cm_pred_testing_opt <- confusionMatrix(as.factor(y_hat_test_optimized),as.factor(y_true_testing)) # misclassification rate = 0.1667


# (f) Repeat parts (b) through (e) using a support vector machine with a radial
# kernel. Use the default value for gamma.

tune.model.rad_kernal <- tune(svm, Purchase ~., data = dataset_OJ_training, kernel = "radial", ranges = list(cost = c(.001, .01, .1, 1, 3, 5, 7, 10)))
summary(tune.model.rad_kernal)
names(tune.model.rad_kernal)
bestmod <- tune.model.rad_kernal$best.model

y_hat_train_optimized_kernal <- (predict(bestmod, newdata = dataset_OJ_training) > 0.5) * 1
y_hat_test_optimized_kernal <- (predict(bestmod, newdata = dataset_OJ_testing) > 0.5) * 1
cm_pred_training_opt_kernal <- confusionMatrix(as.factor(y_hat_train_optimized_kernal), as.factor(y_true_training)) # training error = 0.1525
cm_pred_testing_opt_kenral <- confusionMatrix(as.factor(y_hat_test_optimized_kernal),as.factor(y_true_testing)) # testing error= 0.18520



# (g) Repeat parts (b) through (e) using a support vector machine with a
# polynomial kernel. Set degree = 2.

tune.model.poly_kernal <- tune(svm, Purchase ~., data = dataset_OJ_training, kernel = "polynomial", degree = 2, ranges = list(cost = c(.001, .01, .1, 1, 3, 5, 7, 10)))
summary(tune.model.poly_kernal)
names(tune.model.poly_kernal)
bestmod <- tune.model.poly_kernal$best.model

y_hat_train_optimized_kernal <- (predict(bestmod, newdata = dataset_OJ_training) > 0.5) * 1
y_hat_test_optimized_kernal <- (predict(bestmod, newdata = dataset_OJ_testing) > 0.5) * 1
cm_pred_training_opt_kernal <- confusionMatrix(as.factor(y_hat_train_optimized_kernal), as.factor(y_true_training)) # training error of 0.1738
cm_pred_testing_opt_kenral <- confusionMatrix(as.factor(y_hat_test_optimized_kernal),as.factor(y_true_testing)) # testing error of 0.2296





















