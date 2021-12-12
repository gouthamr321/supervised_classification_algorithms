###############################
# Homework 2
# problem 1
# Gouthamrajan Nadarajan
###############################


rm(list = ls())

library(MASS)
library(leaps)

set.seed(123)

cereal_tbl <- read.csv("/Users/goutham/Development/statistical_data_mining/homework_1/cereal.csv")
# randomly gets 80% of the indicies
train_indis <- sample(c(1:length(cereal_tbl[,1])), size = round(0.80*length(cereal_tbl[,1])), replace = FALSE) 
cereal_data <- cereal_tbl[,4:ncol(cereal_tbl)]
## create train and test set
trianing_dat <- cereal_tbl[train_indis, 4:ncol(cereal_tbl)]
testing_dat <- cereal_tbl[-train_indis,4:ncol(cereal_tbl)]
train_y_label <- trianing_dat$rating
test_y_label <- testing_dat$rating
# fit a linear model
model <- lm(rating ~ ., data = trianing_dat) # linear model training
print(coef(model))
train_predictions <- predict.lm(model, trianing_dat, se_fit=TRUE)
test_predictions <- predict.lm(model, testing_dat, se_fit=TRUE)
# need to compare test_predictions to test_y_label to get the mean square error
# Mean square errot for testing ----- still dont know how to get that for training
train_err_store <- (1/length(train_y_label))*sum((train_y_label-train_predictions)^2) # mean squared error residual sum of square * scale value
test_err_store <- (1/length(test_y_label))*sum((test_y_label-test_predictions)^2)



# Forward subset selection
regfit.fwd <- regsubsets(rating~., data = cereal_data, nbest = 1, nvmax = ncol(cereal_data) - 1, method = "forward") # perform subset selection
regfit.exhaust <- regsubsets(rating~., data = cereal_data, nbest = 1, nvmax = ncol(cereal_data) - 1, method = "exhaustive") # perform subset selection

# function from computational lab that is copied over
predict.regsubsets = function(object, newdata, id){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
forward_subset_training_error <- matrix(rep(NA, ncol(cereal_data) - 1))
forward_subset_testing_error <- matrix(rep(NA, ncol(cereal_data) - 1))

for (i in 1:12){
  # make the predictions
  y_hat_train = predict(regfit.fwd, newdata = trianing_dat, id = i)
  y_hat_test = predict(regfit.fwd, newdata = testing_dat, id = i)
  # compare the prediction with the true
  forward_subset_training_error[i] = (1/length(train_y_label))*sum((train_y_label-y_hat_train)^2) # mean squared error residual sum of square * scale value
  forward_subset_testing_error[i] = (1/length(test_y_label))*sum((test_y_label-y_hat_test)^2)
}


# backward subset selection
exhaustive_subset_training_error <- matrix(rep(NA, ncol(cereal_data) - 1))
exhaustive_subset_testing_error <- matrix(rep(NA, ncol(cereal_data) - 1))

for (j in 1:12){
  y_hat_train = predict(regfit.exhaust, newdata = trianing_dat, id = j)
  y_hat_test = predict(regfit.exhaust, newdata = testing_dat, id = j)
  exhaustive_subset_training_error[j] = (1/length(train_y_label))*sum((train_y_label-y_hat_train)^2) 
  exhaustive_subset_testing_error[j] = (1/length(test_y_label))*sum((test_y_label-y_hat_test)^2)
}




quartz()
plot(forward_subset_training_error, col = "red", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0,50))
lines(forward_subset_testing_error, col = "blue", type = "b",  lty=2)
legend(10, 50, legend=c("training error", "testing error"), col=c("red", "blue"), lty=1:2, cex=0.8)
title("Forward subset mean square error")

quartz()
plot(exhaustive_subset_training_error, col = "red", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0,50))
lines(exhaustive_subset_testing_error, col = "blue", type = "b", lty=2)
legend(10, 50, legend=c("training error", "testing error"), col=c("red", "blue"), lty=1:2, cex=0.8)
title("exhaustive subset mean square error")




#plot(backward_subset_training_error, col = "blue", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0,50))









