library(ISLR2)
data(Boston)

fit <- regsubsets(medv~., data = training, method = "exhaustive", nvmax = ncol(Boston)- 1)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp # tells us that 10 is best
my_summary$bic # tells us that 10 is best
select = summary(fit)$outmat # here the rows are the number of varibales and columns are variables (each * denotes the )









