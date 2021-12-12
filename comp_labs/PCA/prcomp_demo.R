data(iris)
head(iris)

# get data ready for principal components
dats0 <- iris[,1:4]
YY <- iris[,5]

# scale the data
dats <- scale(dats0)

# compute the PCs
pc_ex <- prcomp(dats, center = FALSE, scale = FALSE) # shows the standard deviation for
plot(pc_ex)

## percent of variation explained 
pc_var <- (pc_ex$sdev)^2
per_var_exp <- (pc_var/(sum(pc_var)))*100
barplot(per_var_exp, main = "PC variance explained", ylab = "% variation explained", xlab = "PCs")
# above shows percent of vairation that each pc contains
## percent of variation explained 
pc_var <- (pc_ex$sdev)^2
pc_var/(sum(pc_var))

## plot the scores of PC1 and PC2
head(pc_ex$x) # these are the score
plot(pc_ex$x[,1], pc_ex$x[,2], xlab = "PC1 scores", ylab = "PC2 scores")

##########################################
# Now lets color them by class
## There are three classes, so lets create a string with three colors
my_col <- rep("black", length(dats[,1]))

unique(YY) # lets change the color of setosa and versicole, and leave virginica black
c1 <- which(YY == "setosa") 
my_col[c1] <- "red"
c2 <- which(YY == "versicolor") 
my_col[c2] <- "yellow"

my_col

plot(pc_ex$x[,1], pc_ex$x[,2], xlab = "PC1 scores", ylab = "PC2 scores", col = my_col, main = "Pretty colored score plot")

##########################################
## Lets do a biplot (view the scores and loadings at the same time)
biplot(pc_ex)

##########################################
## Lets do a biplot (view the scores and loadings at the same time)
library(plotly)
plot_ly(x=pc_ex$x[,1], y=pc_ex$x[,2], z=pc_ex$x[,3], type="scatter3d", mode="markers", color=YY)
##(you can move this around and save it as a png)







