
library(ISLR)
library(caret)
library(class)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(psych)
library(MASS)


rm(list = ls())



data = ISLR2::Weekly

summary(data)
lag1 <- data$Lag1
lag2 <- data$Lag2
lag3 <- data$Lag3
lag4 <- data$Lag4
lag5 <- data$Lag5
volume <- data$Volume

lag_data <- data.frame(cbind(lag1, lag2, lag3, lag4, lag5))

# pairs plot of data
pairs.panels(lag_data, 
             hist.col = "cyan",
             density = TRUE,  
             ellipses = TRUE, 
             pch=21
) 


# logistic regression using all the variables
data_regression <- data.frame(cbind(data$Lag1, data$Lag2, data$Lag3, data$Lag4, data$Lag5, data$Volume, data$Direction))
regression_fit <-glm(X7 ~.,data=data_regression)
summary(regression_fit)
y_pred <-  round(predict(regression_fit,type="response",newdata=data_regression))
cm_reg = confusionMatrix(as.factor(y_pred),factor(data_regression$X7))


# logistic regression with only lag2
data_1990_2008 <- data.frame(cbind(data_regression[1:985,2], data_regression[1:985,7]))
data_1990_2008_testing <- data.frame(cbind(data_regression[986:1089,2], data_regression[986:1089,7]))
regression_fit <-glm(X2 ~.,data=data_1990_2008)
y_pred <-  round(predict(regression_fit,type="response",newdata=data_1990_2008_testing))
cm_reg = confusionMatrix(as.factor(y_pred),factor(data_1990_2008_testing[,2]))

# LDA using only lag2
lda_data_1990_2008 = lda(X2~.,data=data_1990_2008)
y_pred <-  predict(lda_data_1990_2008,type="response",newdata=data_1990_2008_testing)$class
cm_reg = confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,2]))

# (f) Using KNN Repeat (d) using KNN with k=1.
y_pred <- knn(train = data.matrix(data_1990_2008[,1]), test = data.matrix(data_1990_2008_testing[,1]), cl = data.matrix(data_1990_2008[,2]), k =1)
knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,2]))$class
cm_reg = confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,2]))

# using KNN model with k=3
y_pred <- knn(train = data.matrix(data_1990_2008[,1]), test = data.matrix(data_1990_2008_testing[,1]), cl = data.matrix(data_1990_2008[,2]), k =5)
knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,2]))$class
cm_reg = confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,2]))

# using logistic regression using both lag1 and lag2
data_1990_2008 <- data.frame(cbind(data_regression[1:985,1:2], data_regression[1:985,7]))
data_1990_2008_testing <- data.frame(cbind(data_regression[986:1089,1:2], data_regression[986:1089,7]))
colnames(data_1990_2008) <- paste("X", 1:3, sep= "")
colnames(data_1990_2008_testing) <- paste("X", 1:3, sep= "")
regression_fit <-glm(X3 ~.,data=data_1990_2008)
y_pred <-  round(predict(regression_fit,type="response",newdata=data_1990_2008_testing))
cm_reg = confusionMatrix(as.factor(y_pred),factor(data_1990_2008_testing[,3]))

# using LDA using both lag1 and lag2
lda_data_1990_2008 = lda(X3~.,data=data_1990_2008)
y_pred <-  predict(lda_data_1990_2008,type="response",newdata=data_1990_2008_testing)$class
cm_reg_2 = confusionMatrix(as.factor(y_pred),as.factor(data_1990_2008_testing[,3]))
