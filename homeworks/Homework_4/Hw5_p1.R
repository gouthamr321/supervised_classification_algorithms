# HOmework 5 question 1 solution


library(rpart)
library(caret)
library(class)
library(randomForest) # install.packages("randomForest")

set.seed(1)

setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/hw5")
load('pendigits.Rdata')

train = sample(1:nrow(pendigits), .80*nrow(pendigits))
pendigits_training = pendigits[train,]
pendigits_testing = pendigits[-train,]


#a) Compute the variance of each of the variables and show that they are very
#similar. How many PCs explain 80% and 90% of the total variation of the
#data? Display biplots for the first few PCs, color the plots by class (digit).
#Create a three-dimensional score plot for PC1, PC2 and PC3, color the
#samples by class.
#b) Divide the data into test and training. Fit a kNN model over a range of “k” to
#the (a) raw data, and (b) PCs from part (A) that capture at least 80% of the
#variation. Comment on your results.
#c) Fit another classifier of your choosing. How do the results compare to part
#(B)?

# Part a: PCA

# calculating the variance of each feature
pendigits_features_dataset <- pendigits[,1:16]
var_store <- c()
for (i in 1:ncol(pendigits_features_dataset)){
  var_store[i] <- var(pendigits_features_dataset[,i])
}


# PCA
pendigits_variables <- pendigits[,-17]
dats <- scale(pendigits_variables) # centers data
pc_ex <- prcomp(dats, center = FALSE, scale = FALSE) # shows the standard deviation for each of the vatiables
plot(pc_ex)
barplot(pc_ex$sdev^2, main="Variance of principal components", xlab="Inputs",ylab = "Variance", names.arg= colnames(pendigits_variables))

# percent of principal components that cover certain percentage of the variation
per_var_exp <- (pc_ex$sdev^2/(sum(pc_ex$sdev^2)))*100
barplot(per_var_exp, main = "PC variance explained", ylab = "% variation explained", xlab = "PCs")

# found that 5 principal comppoenents make up 80% of data

# display biplots for the first few PC
#plot(pc_ex$x[,1], pc_ex$x[,2], xlab = "PC1 scores", ylab = "PC2 scores")
my_col <- rep("black", length(dats[,1]))
classes <- pendigits[,17]
unique(classes)

c1 <- which(classes == 0) 
my_col[c1] <- "green"
c2 <- which(classes == 1) 
my_col[c2] <- "red"
c3 <- which(classes == 2) 
my_col[c3] <- "yellow"
c4 <- which(classes == 3) 
my_col[c4] <- "blue"
c5 <- which(classes == 4) 
my_col[c5] <- "orange"
c6 <- which(classes == 5) 
my_col[c6] <- "purple"
c7 <- which(classes == 6) 
my_col[c7] <- "pink"
c8 <- which(classes == 7) 
my_col[c8] <- "burlywood3"
c9 <- which(classes == 8) 
my_col[c9] <- "blueviolet"

# plot the PC against each other
plot(pc_ex$x[,1], pc_ex$x[,2], xlab = "PC1 scores", ylab = "PC2 scores", col = my_col, main = "PC1 and PC2 plot")
plot(pc_ex$x[,3], pc_ex$x[,4], xlab = "PC3 scores", ylab = "PC4 scores", col = my_col, main = "PC3 and PC4 plot")

# 3d plot with 3 PC
library(plotly)
plot_ly(x=pc_ex$x[,1], y=pc_ex$x[,2], z=pc_ex$x[,3], type="scatter3d", mode="markers", color=classes)

# plotting a biplot
#biplot(pc_ex) # commenting this because this takes a long time

# part b KNN--- fit KNN classifier 
k_values <- 1:10
error <- c()
for (i in 1:length(k_values)){
  y_pred <- knn(train = data.matrix(pendigits_training[,-17]), test = data.matrix(pendigits_testing[,-17]), cl = pendigits_training[,17], k = i)
  cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(pendigits_testing[,17]))
  error[i] <- 1 - cm_knn_fit$overall[1]
}
print(paste0("the KNN with the lowest error is ", which.min(error))) 



# KNN on the Principal components
pc_dataset <- data.frame(cbind(pc_ex$x[,1], pc_ex$x[,2], pc_ex$x[,3], pc_ex$x[,4], pc_ex$x[,5]))
pc_dataset_training = pc_dataset[train,]
pc_dataset_testing = pc_dataset[-train,]

k_values <- 1:10
error_PC <- c()
for (i in 1:length(k_values)){
  y_pred <- knn(train = data.matrix(pc_dataset_training), test = data.matrix(pc_dataset_testing), cl = pendigits_training[,17], k = i)
  cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(pendigits_testing[,17]))
  error_PC[i] <- 1 - cm_knn_fit$overall[1]
}
print(paste0("the KNN for Principal components with the lowest error is ", which.min(error_PC))) 



# part c: Fit Random Forest
rf.fit <- randomForest(as.factor(class)~., data = pendigits_training, n.tree = 100) # use 100 trees for random forest
quartz()
varImpPlot(rf.fit)
importance(rf.fit) # see the importance of each feature
RF_pred_training <- predict(rf.fit, newdata = pendigits_training, type = "response")
RF_pred_testing <- predict(rf.fit, newdata = pendigits_testing, type = "response")
cm_pred_training <- confusionMatrix(as.factor(RF_pred_training), as.factor(pendigits_training$class)) # 0 training error
cm_pred_testing <- confusionMatrix(as.factor(RF_pred_testing), as.factor(pendigits_testing$class)) # 0.0077 testing error

# compare to fitting random forest on principal components
pc_dataset <- data.frame(cbind(pc_dataset, pendigits[,17]))
pc_dataset_training = pc_dataset[train,]
pc_dataset_testing = pc_dataset[-train,]
pc_dataset_training = pc_dataset[train,]
pc_dataset_testing = pc_dataset[-train,]


rf.fit_PC <- randomForest(as.factor(pc_dataset_training[,6])~., data = pc_dataset_training, n.tree = 100)
quartz()
varImpPlot(rf.fit_PC)
importance(rf.fit_PC)
RF_pred_training <- predict(rf.fit_PC, newdata = pc_dataset_training, type = "response")
RF_pred_testing <- predict(rf.fit_PC, newdata = pc_dataset_testing, type = "response")
cm_pred_training <- confusionMatrix(as.factor(RF_pred_training), as.factor(pc_dataset_training[,6])) # 0 training error
cm_pred_testing <- confusionMatrix(as.factor(RF_pred_testing), as.factor(pc_dataset_testing[,6])) # 0 testing error



