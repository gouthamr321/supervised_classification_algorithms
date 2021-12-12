##############################################
# This code is to grow and boost and bag
# classification trees and build random forests.
# Rachael Blair
# Created: October 31, 2014
# Edited: November 2020
##############################################
rm(list = ls())

library(rpart)
library(gbm)
library(randomForest) # install.packages("randomForest")
library(geneplotter)  # bioconductor
library(ISLR)

# Load the data
data(Carseats)
car <- Carseats
 
# Recode sales as a binary respons --- if greater than 8 -- high else low
High <- ifelse(car$Sales<=8, "No", "Yes")
my_car <- data.frame(car[,-1], High) 

# set the seed, and put aside a test set
set.seed(12345)
test_indis <- sample(1:nrow(my_car), .20*nrow(my_car))
test <- my_car[test_indis, ]
training <- my_car[-test_indis, ]

test$High <- as.factor(test$High)
y_true <- as.numeric(test$High)-1 # 0 No, 1 Yes

############################################
# Grow a single tree
############################################
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit <- rpart(High~., data = training, method = "class", control = model.control)

x11()
plot(fit)
text(fit, use.n = TRUE, cex = .5)

# prune the tree back
min_cp = which.min(fit$cptable[,4])

x11()
plot(fit$cptable[,4], main = "Cp for model selection", ylab = "cv error")
saveeps("test")

pruned_fit <- prune(fit, cp = fit$cptable[min_cp, 1])
x11()
plot(pruned_fit)
text(pruned_fit, use.n = TRUE, cex = .5)

# Compute test error for a single tree
my_pred <- predict(pruned_fit, newdata = test, type = "class")
y_hat <- as.numeric(my_pred)-1
misclass_tree <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_tree #.26

###############################################
# Random Forest 
###############################################
training$High <- as.factor(training$High) # this is a change -- has to be factor.
rf.fit <- randomForest(High~., data = training, n.tree = 10000)

x11()
varImpPlot(rf.fit)

importance(rf.fit)

y_hat <- predict(rf.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf # .1875

###############################################
# Bagging
###############################################
bag.fit <- randomForest(High~., data = training, n.tree = 10000, mtry = 10)

x11()
varImpPlot(bag.fit)

importance(bag.fit)

y_hat <- predict(bag.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_bag <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_bag # 0.2125


# here graph is showing the importance of each variable..... y axis has the variables and x is importance.... highest x value is most important

###############################################
# Boosting
###############################################
boost.train <- training;
boost.train$High <- as.numeric(training$High)-1
boost.test <- test;
boost.test$High <- as.numeric(test$High)-1

boost.fit <- gbm(High~., data = boost.train, n.trees = 1000, shrinkage = .1, interaction.depth = 3, distribution = "adaboost")
boost.fit2 <- gbm(High~., data = boost.train, n.trees = 1000, shrinkage = .6, interaction.depth = 3, distribution = "adaboost")

# sh
summary(boost.fit)
# shrinkage is a tree parameter
# Look at the error for shrinkage = .1
y_hat <- predict(boost.fit, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.1 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.1 # 0.1543087

# Look at the error for shrinkage = .6
y_hat <- predict(boost.fit2, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.6 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.6 # 0.120039

shrink <- c(.1, .4, .6, .8)
max_iter <- 5000 # this is the number of trees that are available in the model 
store_error <- c()
for (i in 1:length(shrink)){ 
	boost.fit <- gbm(High~., data = boost.train, n.trees = max_iter, shrinkage = shrink[i], interaction.depth = 3, distribution = "adaboost")
	temp <- c()
	for (j in 1:max_iter){
		y_hat <- predict(boost.fit, newdat = boost.test, n.trees = j, type = 		"response")
		misclass_boost <- sum(abs(y_true - y_hat))/length(y_hat)
		temp <- c(temp, misclass_boost)
	}
	store_error <- cbind(store_error, temp) # max_iter x length(shrink)
}

colnames(store_error) <- paste("shrinkage", shrink, sep = ":")

x11()
plot(store_error[,1], type = "l", main = "Error Profiles", ylab = "error", xlab = "boosting iterations", ylim = c(.07, .5))
lines(store_error[,2], col = "red")
lines(store_error[,3], col = "blue")
lines(store_error[,4], col = "green")

##  Boosting with shrinkage error of: #0.125 

#library(adabag) #multiple classes > 2






