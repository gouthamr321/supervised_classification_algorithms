#### R problem 1


###
# (a)
# create X usig log-normal
# Beta using another type of distribution ---- but override some to make them wero
# add some noise using rnorm function e.g. 0.01 * rnorm(1000)
# Y = Xβ + ϵ


library(MASS)
library(leaps)

set.seed(2)
X <- matrix(rnorm(10* 1000), nrow =1000, ncol=20)
colnames(X) <- paste("X", 1:20, sep= "")
Beta <- matrix(rnorm(20 * 1), nrow =20, ncol=1)
## overriding beta with some zeros
ind <- sample(1:dim(Beta)[1], 5, replace=F)
for (i in 1:length(ind)){ # randomly make beta values
  Beta[i] = 0 
}
error <- matrix(rnorm(1000 * 1) * 0.001, nrow =1000, ncol=1)
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
regfit.fwd <- regsubsets(Y_training~., data = data_training, nbest = 1, nvmax = ncol(data_training) - 1, method = "forward") # perform subset selection


#regfit.exhaust <- regsubsets(rating~., data = cereal_data, nbest = 1, nvmax = ncol(cereal_data) - 1, method = "exhaustive") # perform subset selection

# function from computational lab that is copied over
predict.regsubsets = function(object, newdata, id){
  form = as.formula(object$call[[2]]) # creates a formula
  print(paste0("form is  ", form)) 
  mat = model.matrix(form, newdata) # break into intercept
  coefi = coef(object,id=id) 
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


forward_subset_training_error <- matrix(rep(NA, ncol(X_training)))
forward_subset_testing_error <- matrix(rep(NA, ncol(X_training)))

for (i in 1:ncol(X_training)){
  # make the predictions
  y_hat_train = predict(regfit.fwd, newdata = data_training, id = i)
  #y_hat_test = predict(regfit.fwd, newdata = X_training, id = i)
  # compare the prediction with the true
  #forward_subset_training_error[i] = (1/length(train_y_label))*sum((train_y_label-y_hat_train)^2) # mean squared error residual sum of square * scale value
  #forward_subset_testing_error[i] = (1/length(test_y_label))*sum((test_y_label-y_hat_test)^2)
}






### create train/test datasets
# use predict function from previous lab
# Does the model get the correct number of features---- are they the correct features
# plot the coefficents as a function of n





