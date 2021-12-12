library(MASS)
library(leaps)

set.seed(20)
vals <- rnorm(20* 1000)
X <- matrix(vals, nrow =1000, ncol=20)
colnames(X) <- paste("X", 1:20, sep= "")
Beta <- matrix(runif(20 * 1), nrow =20, ncol=1)
## overriding beta with some zeros
ind <- sample(1:dim(Beta)[1], 5, replace=F)
for (i in 1:length(ind)){ # randomly make beta values = 0
  Beta[ind[i]] = 0 
}
error <- matrix(rnorm(1000 * 1) * 0.05, nrow =1000, ncol=1)
Y <- X%*%Beta + error

# train/test splits
train_indis <- sample(c(1:length(Y[,1])), size = round(0.90*length(Y[,1])), replace = FALSE) 
Y_training <- Y[train_indis,]
Y_testing <- Y[-train_indis,]

X_training <- X[train_indis,]
X_testing <- X[-train_indis,]


# subset selection
#regfit.fwd <- regsubsets(x=X_training, y=Y_training, nbest = 1, nvmax = ncol(X_training), method = "forward") # perform subset selection
data_training <- data.frame(cbind(X_training,Y_training))
data_testing <- data.frame(cbind(X_testing, Y_testing))
names(data_training)[names(data_training) == 'Y_training'] <- "Y"
names(data_testing)[names(data_testing) == 'Y_testing'] <- "Y"
regfit.fwd <- regsubsets(Y~., data = data_training, nbest = 1, nvmax = ncol(data_training) - 1, method = "forward") # perform subset selection

# function from computational lab that is copied over
predict.regsubsets = function(object, newdata, id){
  form = as.formula(object$call[[2]]) # creates a formula
  #print(paste0("form is  ", form)) 
  mat = model.matrix(form, newdata) # break into intercept
  coefi = coef(object,id=id) 
 
  xvars=names(coefi)
  print(paste0('coeff are', xvars))
  mat[,xvars]%*%coefi
}

forward_subset_training_error <- matrix(rep(NA, ncol(X_training)))
forward_subset_testing_error <- matrix(rep(NA, ncol(X_training)))

for (i in 1:ncol(X_training)){
  # make the predictions
  y_hat_train = predict(regfit.fwd, newdata = data_training, id = i)
  y_hat_test = predict(regfit.fwd, newdata = data_testing, id = i)
  # compare the prediction with the true
  forward_subset_training_error[i] = (1/length(Y_training))*sum((Y_training-y_hat_train)^2) # mean squared error residual sum of square * scale value
  forward_subset_testing_error[i] = (1/length(Y_testing))*sum((Y_testing-y_hat_test)^2)
}

quartz()
plot(forward_subset_training_error, col = "blue", type = "b", xlab = "No. of variables", ylab = "MSE", ylim = c(0,5), main="subset selection using simulated data")
lines(forward_subset_testing_error, col = "red", type = "b")
legend("topright", legend=c("training error", "testing error"), col=c("red", "blue"), lty=1:2, cex=0.8)
#which(test_err_store == min(test_err_store)

# part g --- create a plot displaying MSE of coefficents

#Similar function as above but this one returns the learned coefficents instead
predict_coeff_regsubsets = function(object, newdata, id){
  form = as.formula(object$call[[2]]) # creates a formula
  #print(paste0("form is  ", form)) 
  mat = model.matrix(form, newdata) # break into intercept
  coefi = coef(object,id=id) 
  xvars=names(coefi)
  print(paste0('coeff are', xvars))
  mat[,xvars]%*%coefi
  return (coefi)
}

rownames(Beta) <- (paste("X", 1:20, sep= ""))
Beta_t <- t(Beta)
total_error <- 0
coeff_error <- matrix(rep(NA, ncol(X_training)))
for (i in 1:ncol(X_training)){
  # make the predictions
  train_out = data.frame(predict_coeff_regsubsets(regfit.fwd, newdata = data_testing, id = i))
  names_coeff <- row.names(train_out)
  # calculate the error in the coefficents
  error <- sqrt(sum((Beta_t[,names_coeff[2:dim(train_out)[1]]] - train_out[names_coeff[2:dim(train_out)[1]],])^2))
  coeff_error[i] <- error
}
# plot coefficents
quartz()
plot(coeff_error, col = "blue", type = "b", xlab = "No. of variables", ylab = "Coefficent error term", ylim = c(0,0.2), main="Coefficent error")









