
rm(list = ls())

library(MASS)
library(leaps)
library("ggplot2")
library(ISLR)


setwd("/Users/goutham/Development/statistical_data_mining/comp_labs/homework_2")
load("/Users/goutham/Development/statistical_data_mining/comp_labs/homework_2/zip_data/zip.test.RData")
load("/Users/goutham/Development/statistical_data_mining/comp_labs/homework_2/zip_data/zip.train.RData")

zip_train <- data.frame(zip.test) # data frame class makes data easier to work with
zip_test <- data.frame(zip.train)

rows_4_train <- zip_train[zip_train[, 1] == 4, ]
rows_7_train <- zip_train[zip_train[, 1] == 7, ]

rows_4_test <- zip_test[zip_test[, 1] == 4, ]
rows_7_test <- zip_test[zip_test[, 1] == 7, ]

# for linear regression
zip_train_processed_LR <- rbind(rows_4_train, rows_7_train)
zip_test_processed_LR <- rbind(rows_4_test, rows_7_test)
# for modeling
zip_train_processed <- rbind(rows_4_train, rows_7_train)
zip_test_processed <- rbind(rows_4_test, rows_7_test)

zip_train_processed_LR$X1 = (zip_train_processed_LR$X1 > 5) * 1
zip_test_processed_LR$X1 = (zip_test_processed_LR$X1 > 5) * 1
# linear regression
model_lm <- lm(X1 ~ ., data = zip_train_processed_LR)
preds <- predict(model_lm, newdata = zip_test_processed_LR)

preds_test <- predict(model_lm, newdata = zip_test_processed_LR)
preds_train <- predict(model_lm, newdata = zip_train_processed_LR)

preds_train <- (preds_train > 0.5) * 1
preds_test <- (preds_test > 0.5) * 1

summary(model_lm)

# extract only the rows of 4 and 7
# perform linear regression
# perform KNN w k= 1,3,5,7,9,11,13,15
# get rows of only 4 and 7

# K nearest neighbor
require(class) # similar to library function
predict_KNN <- function(train, test, k){ # function in R
  KNN <- knn(train[, -1], test[, -1], train$X1, k) # KNN testing predictions
  KNN_train <- knn(train[, -1], train[, -1], train$X1, k) # KNN training predictions
  predictions <- KNN
  predictions_train <- KNN_train
  output_predictions <- list(predictions_train, predictions)
  return (output_predictions)
}


require(class) # similar to library function
get_error <- function(gt, train_predictions){ # function in R
  # This gets in the format 0 to 1
  train_predictions<- (as.numeric(as.character(train_predictions)) > 5) * 1
  gt <- (gt > 5) * 1
  count =0
  for (i in 1:length(gt)){
    if (train_predictions[i] != gt[i]){
      count = count + 1    # adds 1 for everytime time there isn't a match
    }
  }
  #print(count)
  error_rate <- count/ length(zip_test_processed$X1)
  # 0 are 4's originally and 7's are now 1
  # get ground truth
  # get training predicitons
  return (error_rate)
}

k_values <- c(1,3,5,7,9,11,13,15)

for (i in 1: length(k_values)){
  prediction_results <- predict_KNN(zip_train_processed, zip_test_processed, k_values[i]) # returns in a list testing and training predictions
  print(paste("using k value", k_values[i])) 
  train_predictions <- prediction_results[[1]]
  test_predictions <- prediction_results[[2]]
  KNN_error_rate <- get_error(zip_test_processed$X1, test_predictions)
  KNN_error_rate_training <- get_error(zip_train_processed$X1, train_predictions)
  print(paste("testing error is", KNN_error_rate))
  print(paste("training error is", KNN_error_rate_training))
}


require(class) # similar to library function
get_error_LM <- function(train_predictions, gt){ # function in R
  # This gets in the format 0 to 1
  count =0
  for (i in 1:length(zip_test_processed$X1)){
    #print(paste("i is", i))
    if (is.na(train_predictions[i])){
      count =count + 1
      next
    }
    if (train_predictions[i] != gt[i]){
      
      count = count + 1    
    }
  }
  error_rate <- count/ length(zip_test_processed$X1)
  # 0 are 4's originally and 7's are now 1
  # get ground truth
  # get training predicitons
  return (error_rate)
}

LM_training_error <- get_error_LM(preds_train, zip_train_processed_LR$X1)
LM_testing_error <- get_error_LM(preds_test, zip_test_processed_LR$X1)
# Comparsions --- make a function of this
#test_comp <- test_predictions == test[,1] 