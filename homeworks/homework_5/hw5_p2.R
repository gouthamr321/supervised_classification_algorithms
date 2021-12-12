# HW 5 question 2 solution


library(rpart)
library(caret)
library(randomForest) # install.packages("randomForest")
library(neuralnet)
library(nnet)

setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/hw5")
load('cleveland.Rdata')
set.seed(1)

cleveland_dat <- cleveland[-15]
cleaveland_dat_responce <- as.numeric(cleveland_dat[,14])
cleveland_dat[,14] <- cleaveland_dat_responce - 1
set.seed(10)
train = sample(1:nrow(cleveland_dat), .80*nrow(cleveland_dat))
cleveland_dat_training = cleveland_dat[train,]
cleveland_dat_testing = cleveland_dat[-train,]

# fit a neural net
cleveland_dat_nn <- cleveland_dat

cleveland_dat_nn$gender <- as.numeric(cleveland_dat$gender)
cleveland_dat_nn$cp <- as.numeric(cleveland_dat$cp)
cleveland_dat_nn$fbs <- as.numeric(cleveland_dat$fbs)
cleveland_dat_nn$restecg <- as.numeric(cleveland_dat$restecg)
cleveland_dat_nn$exang <- as.numeric(cleveland_dat$exang)
cleveland_dat_nn$slope <- as.numeric(cleveland_dat$slope)
cleveland_dat_nn$thal <- as.numeric(cleveland_dat$thal)

cleveland_dat_nn_training = cleveland_dat_nn[train,]
cleveland_dat_nn_testing = cleveland_dat_nn[-train,]

#nn1 <- neuralnet(diag1 ~ ., data = cleveland_dat_nn_training, hidden = i, stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
train_err_store <- c()
test_err_store <- c()
for (i in 1:10){
  # fit neural network with "i" neurons --- i is the number of neurons in the hidden layer
  nn1 <- neuralnet(diag1 ~ ., data = cleveland_dat_nn_training, hidden = i, stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
  
  # calculate the train error
  pred <- predict(nn1, newdata = cleveland_dat_nn_training)
  y_hat_train <- round(pred)
  train_err <- length(which(cleveland_dat_nn_training$diag1 != y_hat_train))/length(y_hat_train)
  train_err_store <- c(train_err_store, train_err) #store the error at each iteration
  
  pred <- predict(nn1, newdata = cleveland_dat_nn_testing)
  y_hat_test <- round(pred)
  test_err <- length(which(cleveland_dat_nn_testing$diag1 != y_hat_test))/length(y_hat_test)
  test_err_store <- c(test_err_store, test_err) #store the error at each iteration	
}
train_err_store
test_err_store
print(paste0("the NN with the lowest test error is the model with hidden units= ", which.min(test_err_store))) 

# fitting the best neural network again and getting a confusion matrix
nn_opt <- neuralnet(diag1 ~ ., data = cleveland_dat_nn_training, hidden = which.min(test_err_store), stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
pred <- predict(nn1, newdata = cleveland_dat_nn_testing)
y_hat_test <- round(pred)
cm_pred_testing_nn <- confusionMatrix(as.factor(y_hat_test),as.factor(cleveland_dat_testing$diag1))


# fit a CART model
model.controls <- rpart.control(minbucket = 5, minsplit = 5, xval = 10, cp = 0)
fit_cleveland <- rpart(diag1~., data = cleveland_dat_training, control = model.controls)
min_cp = which.min(fit_cleveland$cptable[,4])
quartz()
# plot to find the best complexity parameter
plot(rev(fit_cleveland$cptable[,1]), rev(fit_cleveland$cptable[,4]), col = "blue", type = "b", xlab = "complexity", ylab = "cross validation error", ylim = c(0,1), main="CART error vs. complexity parameter")
pruned_fit_cleveland <- prune(fit_cleveland, cp = fit_cleveland$cptable[min_cp, 1])

plot(pruned_fit_cleveland, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_cleveland, cex = .5)
CART_pred_training <- predict(pruned_fit_cleveland, newdata = cleveland_dat_training) 
CART_pred_training <- (CART_pred_training > 0.5) * 1
cm_pred_training <- confusionMatrix(as.factor(CART_pred_training),as.factor(cleveland_dat_training$diag1))

CART_pred_testing <- predict(pruned_fit_cleveland, newdata = cleveland_dat_testing) 
CART_pred_testing <- (CART_pred_testing > 0.5) * 1
cm_pred_testing <- confusionMatrix(as.factor(CART_pred_testing),as.factor(cleveland_dat_testing$diag1))

# fit a Random forest
rf.fit <- randomForest(as.factor(diag1)~., data = cleveland_dat_training, n.tree = 10000) # using num tree = 1000
quartz()
varImpPlot(rf.fit)
importance(rf.fit)
RF_pred_training <- predict(rf.fit, newdata = cleveland_dat_training, type = "response")
RF_pred_testing <- predict(rf.fit, newdata = cleveland_dat_testing, type = "response")
cm_pred_training <- confusionMatrix(as.factor(RF_pred_training),as.factor(cleveland_dat_training$diag1)) # 0% training error 
cm_pred_testing <- confusionMatrix(as.factor(RF_pred_testing),as.factor(cleveland_dat_testing$diag1)) # 0.1333 testing error





