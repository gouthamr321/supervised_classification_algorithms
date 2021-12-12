library(ISLR2)
library(caret)
library(class)
library(MASS)
library(boot)  #install.packages("bootstrap")
library(ggplot2)

data(Auto)

# part (a) -- mpg01 binary variable if greater than median and otherwise 0
Auto_dat <- Auto

Auto_mpg <- Auto$mpg
Auto_mpg <- (Auto_mpg > median(Auto_mpg)) * 1
Auto_dat$mpg <- Auto_mpg

ind_0 <- which(Auto_dat$mpg == 0)
ind_1 <- which(Auto_dat$mpg == 1)
# part b --- visual graphs --- im thinking scatterplots where we plot each of the 9 variables according
#quartz()
#plot(Auto_dat$cylinders[ind_0], Auto_dat$mpg[ind_0], main="Scatterplot Example",
#     xlab="Cylinders ", ylab="mpg")
#lines(Auto_dat$cylinders[ind_1], Auto_dat$mpg[ind_1])

# Boxplot of MPG by Car Cylinders

boxplot(cylinders~mpg,data=Auto_dat, main="num cylinders vs. mpg", ylab="Number of Cylinders", xlab="mpg")
boxplot(displacement~mpg,data=Auto_dat, main="displacement vs. mpg", ylab="displacement", xlab="mpg")
boxplot(horsepower~mpg,data=Auto_dat, main="horsepower vs. mpg", ylab="Number of horsepower", xlab="mpg")
boxplot(weight~mpg,data=Auto_dat, main="weight vs. mpg", ylab="weight", xlab="mpg")
boxplot(acceleration~mpg,data=Auto_dat, main="acceleration vs. mpg", ylab="acceleration", xlab="mpg")
boxplot(year~mpg,data=Auto_dat, main="year vs. mpg", ylab="year", xlab="mpg")
boxplot(origin~mpg,data=Auto_dat, main="origin vs. mpg", ylab="origin", xlab="mpg")


# divide into 80:20 train/test split

# first need to take boston data and replace with binary values.... crime rate median as threshold
train = sample(1:nrow(Auto_dat), .80*nrow(Auto_dat))
training = Auto_dat[train,c(1,2,3,8)]
testing = Auto_dat[-train,c(1,2,3,8)]

# LDA
regression_fit <-lda(mpg ~.,data=training)
y_pred_lda <-  predict(regression_fit, type="response", newdata=testing)
lda_out <- y_pred_lda$class
cm_reg_lda_model = confusionMatrix(as.factor(lda_out),as.factor(testing$mpg))

# QDA
qda_fit = qda(mpg ~.,data=training)
qda_pred <-predict(qda_fit,newdata=testing)
qda_out <- qda_pred$class
qda_cm_test<- confusionMatrix(as.factor(qda_out),as.factor(testing$mpg))

# logistic regression
regression_fit <-glm(mpg ~.,data=training)
summary(regression_fit)
y_pred_glm <-  round(predict(regression_fit, type="response", newdata=testing))
cm_reg_logistic_regression_model = confusionMatrix(as.factor(y_pred_glm),as.factor(testing$mpg))

# KNN
k_values <- 1:10
error <- c()
for (i in 1:length(k_values)){
  y_pred <- knn(train = data.matrix(training[,-1]), test = data.matrix(testing[,-1]), cl = training$mpg, k = i)
  cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(testing$mpg))
  error[i] <- 1 - cm_knn_fit$overall[1]
}

# plot of error vs. k values
quartz()
plot(error, col = "blue", type = "b", xlab = "Value for k", ylab = "Error", ylim = c(0,0.15), main="Auto KNN model using different values of k")
print(paste0("the KNN with the lowest error is ", which.min(error))) 

# found best model is k=3 in this case
y_pred <- knn(train = data.matrix(training[,-1]), test = data.matrix(testing[,-1]), cl = training$mpg, k = 3)
cm_knn_fit <- confusionMatrix(as.factor(y_pred),as.factor(testing$mpg))



