library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(psych)
library(caret)
library(MASS)

rm(list=rm())
#load in Diabetes data
setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/lab3")
load("Diabetes.RData")
diabetes_proc<-Diabetes[,-6]

# pairs plot
pairs.panels(diabetes_proc, 
             hist.col = "cyan",
             density = TRUE,  
             ellipses = TRUE, 
             col = Diabetes$group,
             pch=21
) 

lda_fit = lda(group~.,data=Diabetes)
qda_fit = qda(group~.,data=Diabetes)

# LDA model training and evaluation
lda_train <-predict(lda_fit,newdata=Diabetes)
lda_out <- lda_train$class
lda_cm_train <- confusionMatrix(as.factor(lda_out),as.factor(Diabetes$group))

# QDA model training and evaluation
qda_train <-predict(qda_fit,newdata=Diabetes)
qda_out <- qda_train$class
qda_cm_train <- confusionMatrix(as.factor(qda_out),as.factor(Diabetes$group))

# make new individual(data points) according to question 3c
data_new_point <- cbind(68, 122, 544, 1.86, 184)
colnames(data_new_point) <- c(colnames(diabetes_proc))
testing_data <- data.frame(data_new_point)

# evaluate individual using trained lda and qda models to get predicted class
testing_lda <-predict(lda_fit, newdata=testing_data)$class
testing_qda <-predict(qda_fit, newdata=testing_data)$class


