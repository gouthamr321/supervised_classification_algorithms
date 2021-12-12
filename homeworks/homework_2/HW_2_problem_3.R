
rm(list = ls())

library(MASS)
library(leaps)
library("ggplot2")
library(ISLR)
library(glmnet)  #install.packages("glmnet)


data("College")

#college_dat <- data.frame(data("College"))

college_dat<- College
#college_dat <- college_dat[,-1]
# split dataset into a train and test
train_indis <- sample(c(1:length(college_dat[,1])), size = round(2/3*length(college_dat[,1])), replace = FALSE) # gives training indicies based on random sampling

train = college_dat[train_indis, ] # these are rows
test = college_dat[-train_indis, ] # these are rows
y_true_train = train$Apps # model gt --- responce variable
y_true_test = test$Apps # model gt ---- responce variable
# normalize some of the variables i.e. number of applications accepted, 
train_OG <- train
test_OG <- test
train$Accept <- train_OG$Accept / train_OG$Apps
test$Accept <- test_OG$Accept / test_OG$Apps

train$Enroll <- train_OG$Enroll / train_OG$Accept
test$Enroll <- test_OG$Enroll / test_OG$Accept

#train$F.Undergrad <- train_OG$F.Undergrad  / train_OG$Enroll
#test$F.Undergrad <- test_OG$F.Undergrad  / test_OG$Enroll

#train$P.Undergrad <- train_OG$P.Undergrad  / train_OG$Enroll
#test$P.Undergrad <- test_OG$P.Undergrad  / test_OG$Enroll

model_lm <- lm(Apps ~ ., data = train)
print(paste0("MSE of training error is ", mean(model_lm$residuals^2)))
MSE_test <- mean((test$Apps - predict.lm(model_lm, test)) ^ 2)
MSE_test_each <- data.frame((test$Apps - predict.lm(model_lm, test)) ^ 2)


# get 10 most colleges with the highest error
MSE_test_each[order(MSE_test_each, decreasing=TRUE)[1:10],,drop=FALSE]


# Ridge regression
train_data <- college_dat[-1]
train_data <- train_data[-1]

Y <- college_dat[2]

ridge.mod = glmnet(train_data, unlist(Y), alpha=0)
train_ind <- sample(1:nrow(college_dat), round(nrow(college_dat)/2))
cv.out <- cv.glmnet(data.matrix(train_data[train_ind,]), Y[train_ind,], alpha = 0)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = data.matrix(train_data[-train_ind,]), type = "response")
y_hat <- ridge.pred2
y_true <- Y[-train_ind,]
test_error_ridge <- mean((y_hat - y_true)^2)  #test_error


# Lasso Regression
#train_data_proc <- college_dat[-1]
ridge.mod = glmnet(train_data,unlist(Y), alpha=1)
train_ind <- sample(1:nrow(college_dat), round(nrow(college_dat)/2))
cv.out <- cv.glmnet(data.matrix(train_data[train_ind,]), Y[train_ind,], alpha = 1)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
ridge.pred <- predict(ridge.mod, s= bestlam, type = "coefficients")
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = data.matrix(train_data[-train_ind,]), type = "response")

y_hat <- ridge.pred2
y_true <- Y[-train_ind,]
test_error_lasso <- mean((y_hat - y_true)^2)  #test_error















