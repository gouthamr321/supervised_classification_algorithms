library(ISLR2)
library(caret)
library(class)
library(MASS)
library(rpart)
library(leaps)
library(bootstrap)


data(Boston)

# divide into training and testing
set.seed(10)
train = sample(1:nrow(Boston), .80*nrow(Boston))
Y.train = Boston$medv[train]
Y.test = Boston$medv[-train]
X.train = Boston[train,]
X.test = Boston[-train,]
training = Boston[train,]
testing = Boston[-train,]

# perform exhaustive subset selection
fit <- regsubsets(medv~., data = training, method = "exhaustive", nvmax = ncol(Boston)- 1)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp # tells us that 10 is best
my_summary$bic # tells us that 10 is best



# cp and bic
# get best ones
which.min(my_summary$cp)
print(paste0("best model using cp is ", which.min(my_summary$cp)))

quartz()
plot(my_summary$cp, type = "o", col = "blue" , xlab = "number of variables", ylab = "cp", main = "cp error per feature")

which.min(my_summary$bic)
print(paste0("best model using bic is ", which.min(my_summary$bic)))

quartz()
plot(my_summary$bic, type = "o", col = "blue" , xlab = "number of variables", ylab = "bic", main = "bic error per feature")


select = summary(fit)$outmat # here the rows are the number of varibales and columns are variables (each * denotes the )
#train.error.store <- c()
#test.error.store <- c()

# investigate 5-fold cross validation
k_fold_1 = 5
rows_per_fold = floor(nrow(Boston)/k_fold_1)
cross_val_5_train.error.store <- matrix(, nrow = ncol(Boston)-1, ncol = 5)
cross_val_5_test.error.store <- matrix(, nrow = ncol(Boston)-1, ncol = 5)
range_var = ncol(Boston) - 1


for (k in 1:range_var){
  temp <- which(select[k,] == "*")  # gets variable(s) that is best
  #temp <- temp + 1 # because responce is included in training in first column
  Boston_var_selected <- Boston[, c(14,temp)] 
  for (i in 1:k_fold_1){
    testing_ind <- (((i-1) * rows_per_fold) + 1): (i * rows_per_fold)
    testing_dat <- Boston_var_selected[testing_ind,]
    training_dat <- Boston_var_selected[-testing_ind,]
    red.fit <- lm(medv~., data = training_dat)
    pred.train = predict(red.fit, newdata = training_dat)
    pred.test = predict(red.fit, newdata = testing_dat)
    test.error <- (1/length(testing_dat$medv))*sum((pred.test - testing_dat$medv)^2)
    train.error <- (1/length(training_dat$medv))*sum((pred.train - training_dat$medv)^2)
    cross_val_5_train.error.store[k,i] <- train.error
    cross_val_5_test.error.store[k,i] <- test.error
  }
}
mean_test_error_5_fold <- rowMeans(cross_val_5_train.error.store) # 5 fold cross validation tells us that 6 variable model is best
print(paste0("best model as evaluated by 5 fold cross validation is ",which.min(mean_test_error_5_fold)))
quartz()
plot(mean_test_error_5_fold, type = "o", col = "blue" , xlab = "number of variables", ylab = "error", main = "Average error for each variable(across 5 folds)")


# investigate 10-fold cross validation

k_fold_2 = 10
rows_per_fold = floor(nrow(Boston)/k_fold_2)
cross_val_10_train.error.store <- matrix(, nrow = ncol(Boston)-1, ncol = 10)
cross_val_10_test.error.store <- matrix(, nrow = ncol(Boston)-1, ncol = 10)
for (k in 1:range_var){
  temp <- which(select[k,] == "*")  # gets variable(s) that is best
  Boston_var_selected <- Boston[, c(14,temp)] 
  for (i in 1:k_fold_2){
    testing_ind <- (((i-1) * rows_per_fold) + 1): (i * rows_per_fold)
    testing_dat <- Boston_var_selected[testing_ind,]
    training_dat <- Boston_var_selected[-testing_ind,]
    red.fit <- lm(medv~., data = training_dat)
    pred.train = predict(red.fit, newdata = training_dat)
    pred.test = predict(red.fit, newdata = testing_dat)
    test.error <- (1/length(testing_dat$medv))*sum((pred.test - testing_dat$medv)^2)
    train.error <- (1/length(training_dat$medv))*sum((pred.train - training_dat$medv)^2)
    cross_val_10_train.error.store[k,i] <- train.error
    cross_val_10_test.error.store[k,i] <- test.error
  }
}

mean_test_error_10_fold <- rowMeans(cross_val_10_train.error.store) # 10 fold cross validation tells us that 6 variable model is best
print(paste0("best model as evaluated by 10 fold cross validation is ",which.min(mean_test_error_10_fold)))
quartz()
plot(mean_test_error_10_fold, type = "o", col = "blue" , xlab = "number of variables", ylab = "error", main = "Average error for each variable(across 10 folds)")




## investigate bootstrap
beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# get x and y
Boston_y <- unlist(Boston[14]) # issue with this make it an array
Boston_x <- Boston[-14]



error_store <- c()
select = summary(fit)$outmat # returns best of subsets
for (i in 1:nrow(select)){
  # Pull out the model 
  temp <- which(select[i,] == "*")
  res <- bootpred(Boston_x[,temp], Boston_y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store <- c(error_store, res[[3]])
}

# plotting .632 estimate
quartz()
plot(error_store, type = "o", col = "blue" , xlab = "number of variables", ylab = "error", main = "Model Selection using .632 error")

print(paste0("best performing model according to .632 estimate is ",which.min(error_store)))











