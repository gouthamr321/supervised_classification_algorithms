#################################################
# This code demonstrates SVM fitting in R
# Rachael Hageman Blair
# Created: December 2020
# Edited: 
#################################################

rm(list = ls())
library(ISLR)
library(e1071)
library(MASS)
library(ROCR)

# load the data
data(Auto)

# Discretize the data
med <- median(Auto$mpg) # 22.75
Good_mpg <- med
low <- which(Auto$mpg < med)
high <- which(Auto$mpg > med)
Good_mpg[low] <- "low"
Good_mpg[high] <- "high"
Good_mpg <- as.factor(Good_mpg)

# modify the data
my_auto <- data.frame(Good_mpg, Auto[,-c(1,8,9)])

# Divide the data into training and test
set.seed(123)
test_indis <- sample(1:nrow(my_auto), 1/3*nrow(my_auto), replace = FALSE)
test <- my_auto[test_indis, ]
training <- my_auto[-test_indis, ]

#######################################
## SVM with a linear kernel
#######################################
fit <- svm(Good_mpg~., data = training, kernel = "linear", cost = .1, scale = FALSE)
fit$index
plot(fit, data = training, formula = displacement ~ horsepower, fill = FALSE)

tune.model <- tune(svm, Good_mpg~., data = training, kernel = "linear",
ranges = list(.001, .01, .1, 1, 5, 10, 20, 30))
summary(tune.model)
bestmod <- tune.model$best.model # tells you what the best model is

# predict the test data 
y_hat <- predict(bestmod, newdata = test)
y_true <- test$Good_mpg
acc <- length(which(y_hat == y_true))/length(y_true)
acc

table(y_hat, y_true)

#################################
## SVM with a radial kernel
#################################
tune.model.rad <- tune(svm, Good_mpg ~., data = training, kernel = "radial",
ranges = list(cost = c(.001, .01, .1, 1, 5, 10), gamma = c(.1, .5, 1, 2, 3, 4)))

summary(tune.model.rad)
names(tune.model.rad)
bestmod <- tune.model.rad$best.model

y_hat <- predict(bestmod, newdata = test)
y_true <- test$Good_mpg
acc.rad <- length(which(y_hat == y_true))/length(y_true)
acc.rad

library(caret)
confusionMatrix(y_hat, y_true)

###############################
## ROC -- using the SVM w/radial model
###############################
p <- tune.model.rad$best.parameters
p[1] # these are the best parameters---> cost
p[2] # this is the best parameter---> gamma

fit <- svm(Good_mpg~., data = training, kernel = "radial", cost = p[1], gamma = p[2], decision.values = TRUE, probability = TRUE)
fit$decision.values # this is the model predictions
hist(fit$decision.values)

# rename the outcome -- into 0-1 getting it ready for ROC
training$Good_mpg
as.numeric(training$Good_mpg) # high = 1, low = 2
as.numeric(training$Good_mpg)-1 # high = 0, low = 1
y_true <- as.numeric(training$Good_mpg)-1

# look at the ROC for training
my_pred <- attributes(predict(fit, newdata = training, probability = TRUE))$probabilities
my_pred <- data.frame(my_pred)
my_pred <- my_pred[,1]

pred <- prediction(my_pred, y_true)
perf <- performance(pred, "tpr", "fpr")
perf_train <- perf

plot(perf_train, colorize = TRUE)

acc.perf <- performance(pred, measure = "acc")
plot(acc.perf)



# look at the ROC for test
y_true <- as.numeric(test$Good_mpg)-1

my_pred <- attributes(predict(fit, newdata = test, probability = TRUE))$probabilities
my_pred <- data.frame(my_pred)
my_pred <- my_pred[,1]

pred <- prediction(my_pred, y_true)
perf <- performance(pred, "tpr", "fpr")
perf_test <- perf

# Plot the ROC of test and training
par(mfrow = c(1,2))
plot(perf_train, colorize = TRUE)
plot(perf_test, colorize = TRUE)























