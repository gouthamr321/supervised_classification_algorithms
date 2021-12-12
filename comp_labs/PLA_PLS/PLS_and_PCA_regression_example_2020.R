#########################################################################################
## This code is perform pca and pls.
## Rachael Blair 
## Created: September 23, 2013
## Edited: September 2020
#########################################################################################

rm(list = ls())

# load the libraries
#install.packages("pls")
library(pls)
#Or use "prcomp" DO NOT USE "princomp".

# set working directory
setwd("~/Desktop/DataMining_Fall2013/regression_codes")

##############################################
## Load the Data
## Reformat to get rid of missing values
##############################################
#load("Hitters.RData")
library(ISLR)
data(Hitters)

my_hitters <- as.matrix(Hitters)
NAmat = matrix(as.numeric(is.na(my_hitters)), ncol =20)
nonNAdx = which(rowSums(NAmat) == 0)
Hitters <- Hitters[nonNAdx, ] # the data we will model

##########################################
# Principal Components Regression
##########################################
set.seed(2)
pcr.fit = pcr(Salary ~. , data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

# Divide data into test and training 
train = sample(1:nrow(Hitters), nrow(Hitters)*.80)
test = -train
y.test = Hitters$Salary[test]
y.train = Hitters$Salary[train]

pcr.fit = pcr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "none")
summary(pcr.fit)

# Evaluate performance of the model with "i" components in the pca regression for test and training.
#?predict.mvr
training_error_store <- c()
test_error_store <- c()
for (i in 1:19){
	pcr.pred.train = predict(pcr.fit, Hitters[train,], ncomp = i)
	pcr.pred.test = predict(pcr.fit, Hitters[test,], ncomp = i)
	train.error <- mean((pcr.pred.train-y.train)^2)
	test.error <- mean((pcr.pred.test-y.test)^2)
	training_error_store <- c(training_error_store, train.error)
	test_error_store <- c(test_error_store, test.error)
}

quartz() #x11()
plot(training_error_store)

quartz()
plot(test_error_store)

#########################################
## Partial Least Squares 
#########################################
pls.fit = plsr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")










