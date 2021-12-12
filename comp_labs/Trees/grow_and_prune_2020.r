#######################################
# This code will: grow, and prune, 
# Classification and # Regression trees
# Rachael Blair 
# Created: November 5, 2017
# Edited: 2020
#######################################

rm(list=ls())
setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/Trees")

library(rpart) #install.packages("rpart")
library(MASS)

# rpart plotting is not good

######################
# 2020 - this part has changed
data(iris)
colnames(iris)[5] <- "type" # necessary for syntax below
data(Boston)
boston <- Boston # necessary for synatax below
# 2020 - end change
######################

# rpart cp is a threshord for pruning 
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0) # minsplit - how many observations must exist for a split
# xval - number of cross validations
# cp - complexity parameter
fit.iris <- rpart(type~., data = iris, method = "class", control = model.control)

quartz()
plot(fit.iris, uniform = T, compress = T)
text(fit.iris, cex = 1)

quartz()
plot(fit.iris, uniform = T, compress = T)
text(fit.iris, use.n = T, all = T, cex = 1) # same thing but display a little more information

quartz()
plot(fit.iris, branch = .4, uniform = T, compress = T) # make branches look a little different
text(fit.iris, use.n = T, all = T, cex = 1)

#############################################
## grow a classification tree and prune it.
#############################################
load("digging_data.RData") #dig_dats / name of object
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.dig <- rpart(Y~., data = dig_dats, method = "class", control = model.control)

quartz()
plot(fit.dig$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(fit.dig$cptable[,4]) # find optimal cp based on cross validation--corresponds to 4
pruned_fit_dig <- prune(fit.dig, cp = fit.dig$cptable[min_cp,1]) # recursively snipp of least important splits based on complexity parameter

## plot the full tree and the pruned tree
quartz()
plot(pruned_fit_dig, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_dig, cex = .5)

quartz()
plot(fit.dig, branch = .3, compress=T, main = "Full Tree")
text(fit.dig, cex = .5)

#############################
### Grow regression trees
#############################
model.controls <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0)
fit_boston <- rpart(medv~., data = boston, control = model.controls)

min_cp = which.min(fit_boston$cptable[,4])
pruned_fit_boston <- prune(fit_boston, cp = fit_boston$cptable[min_cp, 1])

quartz()
plot(fit_boston$cptable[,4], main = "Cp for model selection", ylab = "cv error")

quartz()
plot(pruned_fit_boston, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_boston, cex = .5)

quartz()
plot(fit_boston, branch = .3, compress=T, main = "Full Tree")
text(fit_boston, cex = .5)

pred_train <- predict(pruned_fit_boston, newdata = boston) # predict raw values in other words.... this is the mean


#predict.rpart()




























