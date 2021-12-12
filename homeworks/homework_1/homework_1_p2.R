rm(list=ls())
setwd("/Users/goutham/Development/statistical_data_mining/homework_1")

library("DAAG")
library("lattice")
library("MASS")
library("geneplotter")
library("gdata")     

library(Hmisc)

library(ISLR2)


# ELiminating outliers as done in problem 1
tbl <- read.csv("cereal.csv")
numeric_table_dat <- tbl[,-(1:3)]
eliminated_outliers <- numeric_table_dat
outliers <- which(numeric_table_dat$sugars< 0 | numeric_table_dat$potass< 0) # which command (dataset rows where weight > 200)
model <- lm(rating ~ ., data = eliminated_outliers) # linear model training

summary(model) 
print(coef(model)) # see coefficent values of model for question 2a

# Using * method
model_2 <- lm(rating ~ carbo*protein)
summary(model_2)

# Using : method
model_3 <- lm(rating ~ carbo:protein)
summary(model_3)
#plot(boston_dataset_mod[,8], boston_dataset_mod[,1], xlab="weighted mean of distances to five Boston employment centres", ylab="crime", main="crime vs. distance to employment centres")

