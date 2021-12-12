library(ISLR2)
library(caret)
library(class)
library(MASS)
library(rpart)

data(Boston)


# divide into train/test split
set.seed(10)
train = sample(1:nrow(Boston), .80*nrow(Boston))
# first need to take boston data and replace with binary values.... crime rate median as threshold
crim_dat <- Boston$crim
crim_dat > median(crim_dat)
new_crim_dat <- (crim_dat > median(crim_dat)) * 1
Boston$crim <- new_crim_dat
Y.train = Boston$crim[train]
Y.test = Boston$crim[-train]
X.train = Boston[train,]
X.test = Boston[-train,]
training = Boston[train,]
testing = Boston[-train,]

# logistic regression
regression_fit <-glm(crim ~.,data=training)
summary(regression_fit)
y_pred_glm <-  round(predict(regression_fit, type="response", newdata=testing))
cm_reg_logistic_regression_model = confusionMatrix(as.factor(y_pred_glm),as.factor(testing$crim))

# LDA analysis
lda_model = lda(crim ~.,data=training)
y_pred_LDA <-  predict(lda_model,type="response",newdata=testing)$class
cm_reg_lda_model = confusionMatrix(as.factor(y_pred_LDA),as.factor(testing$crim))

# KNN ---- should I try multiple values of k ? --- yes trying k values from 1 through 10
k_values <- 1:10
error <- c()
for (i in 1:length(k_values)){
  y_pred <- knn(train = data.matrix(training[,-1]), test = data.matrix(testing[,-1]), cl = training$crim, k = i)
  cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(testing$crim))
  error[i] <- 1 - cm_knn_fit$overall[1]
}

# plot of error vs. k values
quartz()
plot(error, col = "blue", type = "b", xlab = "Value for k", ylab = "Error", ylim = c(0,0.15), main="KNN model using different values of k")
print(paste0("the KNN with the lowest error is ", which.min(error))) 

# results of best model for k=3
y_pred <- knn(train = data.matrix(training[,-1]), test = data.matrix(testing[,-1]), cl = training$crim, k = 3)
cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(testing$crim))


# CART model
model.controls <- rpart.control(minbucket = 2, minsplit = 40, xval = 10, cp = 0)
fit_boston <- rpart(crim~., data = training, control = model.controls)
min_cp = which.min(fit_boston$cptable[,4])
quartz()
# plot to find the best complexity parameter
plot(rev(fit_boston$cptable[,1]), rev(fit_boston$cptable[,4]), col = "blue", type = "b", xlab = "complexity", ylab = "cross validation error", ylim = c(0,1), main="CART error vs. complexity parameter")
pruned_fit_boston <- prune(fit_boston, cp = fit_boston$cptable[min_cp, 1])

# see prunned tree
quartz()
plot(pruned_fit_boston, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_boston, cex = .5)

pred_training <- predict(pruned_fit_boston, newdata = training) 
pred_testing <- predict(pruned_fit_boston, newdata = testing) # predict raw values in other words.... this is the mean

# get testing error
pred_testing <- (pred_testing > 0.5) * 1
cm_pred_testing <- confusionMatrix(as.factor(pred_testing),as.factor(testing$crim))















