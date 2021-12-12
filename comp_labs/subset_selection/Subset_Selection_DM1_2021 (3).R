##########################################################################
## This code is perform subset selection using different methods.
## Rachael Blair 
## Created: July, 2021
## Edited:
##########################################################################
rm(list = ls())

library(MASS)
library(leaps)

# load a dataset
data(Boston)
dim(Boston)
head(Boston)

# Perform subset selection on the Boston data
regfit.full <- regsubsets(medv~., data = Boston, nbest = 1, nvmax = 13, method = "exhaustive")
my_sum <- summary(regfit.full)
names(my_sum)

# plot model selection measures
par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "No. of variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "No. of variables", ylab = "BIC", type = "l")
plot(my_sum$rss, xlab = "No. of variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "No. of variables", ylab = "Adjusted Rsq", type = "l")

# identify the optimal models using model selection measures
which(my_sum$cp == min(my_sum$cp)) #11 var
which(my_sum$bic == min(my_sum$bic)) #11 var
which(my_sum$rss == min(my_sum$rss)) #13 var
which(my_sum$adjr2 == max(my_sum$adjr2)) #11 var

# Forward and Backwards Subset Selection
regfit.fwd <- regsubsets(medv~., data = Boston, nbest = 1, nvmax = 13, method = "forward")
regfit.bwd <- regsubsets(medv~., data = Boston, nbest = 1, nvmax = 13, method = "backward")

my_sum_fwd <- summary(regfit.fwd)
my_sum_bwd <- summary(regfit.bwd)

# examine the best "p" variables models
my_sum_fwd$outmat
my_sum_bwd$outmat

my_sum_fwd$outmat[7,]
my_sum_bwd$outmat[7,]

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

######################################################
# Look at subset selection using test/training data
######################################################
# what does this function do?---- it takes in object, id and somehow outputs coefficents
predict.regsubsets = function(object, newdata, id){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}

# create test and training
set.seed(123)
train_indis <- sample(c(1:length(Boston[,1])), size = round(2/3*length(Boston[,1])), replace = FALSE) # gives training indicies based on random sampling

train = Boston[train_indis, ] # these are rows
test = Boston[-train_indis, ] # these are rows
y_true_train = train$medv # model gt --- responce variable
y_true_test = test$medv # model gt ---- responce variable

# create objects to store error
train_err_store <- matrix(rep(NA, 13))
test_err_store <- matrix(rep(NA, 13))
regfit.fwd <- regsubsets(medv~., data = Boston, nbest = 1, nvmax = 13, method = "forward") # perform subset selection
for (i in 1:12){
    # make the predictions
    y_hat_train = predict(regfit.fwd, newdata = train, id = i)
    y_hat_test = predict(regfit.fwd, newdata = test, id = i)
    
    # compare the prediction with the true
    train_err_store[i] = (1/length(y_true_train))*sum((y_true_train-y_hat_train)^2) # mean squared error residual sum of square * scale value
    test_err_store[i] = (1/length(y_true_test))*sum((y_true_test-y_hat_test)^2)
}

quartz()
plot(train_err_store, col = "blue", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0,50))
lines(test_err_store, col = "red", type = "b")
which(test_err_store == min(test_err_store))
























    




















































