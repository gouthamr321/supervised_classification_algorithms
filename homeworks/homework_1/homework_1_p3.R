
rm(list=ls())
setwd("/Users/goutham/Development/statistical_data_mining/homework_1")

library("DAAG")
library("lattice")
library("MASS")
library("geneplotter")
library("gdata")     
library(Hmisc)
library(ISLR2)

boston_dataset <- Boston

boston_dataset_mod <- Boston

# plotting scatterplots that I had thought would show a relation
plot(boston_dataset_mod[,1], boston_dataset_mod[,10], xlab = "crime", ylab= "tax", main="tax vs. crime")
plot(boston_dataset_mod[,1], boston_dataset_mod[,3], xlab = "crime", ylab = "proportion of non-retail business acres per town", main="prop non-retail buisiness vs. crime")
plot(boston_dataset_mod[,10], boston_dataset_mod[,13], xlab="tax", ylab="median value of owner-occupied homes in $1000s", main="home value vs. tax")
plot(boston_dataset_mod[,11], boston_dataset_mod[,6], xlab="pupil-teacher ratio by town.", ylab="number of rooms per dwelling", main="number of rooms vs. pupil-teacher ratio")
plot(boston_dataset_mod[,8], boston_dataset_mod[,1], xlab="weighted mean of distances to five Boston employment centres", ylab="crime", main="crime vs. distance to employment centres")  # THis seems to be correlated with crime rate...... further away from employment centers, the less the crime is


# Do certian suburbs have particularly (1) high crime rates, (2) Tax rates, (3) Pupil-Teacher ratios that are high
#for (1) --- high crime suburbs are 381, 406, and 419
high_crime_suburbs <- which(boston_dataset$crim> 60)

# for (2) ---- high tax suburbs are 489, 490, 491, 492, and 493
high_tax <- which(boston_dataset$tax > 710)

# for (3) --- pupil-teach ration high are 
high_ptration <- which(boston_dataset$ptratio > 21.5) # 355 and 356
#range_of_each predictor
# for (1)--- it is in range 0-90
# for (2) --- it is in range 150-750
# for (3) --- it is in range 12- su


# how many of the suburbs average more than 7 and 8 rooms per dwelling
seven_room_dwellings <- which(boston_dataset$rm > 7) # 64 suburbs 
eight_room_dwellings <- which(boston_dataset$rm > 8) # 13 suburbs

# find out the mean of the variables of the seven and eight room dwellings for other varibales
seven_room_dwellings_dat <- boston_dataset[seven_room_dwellings,]
eight_room_dwellings_dat <- boston_dataset[eight_room_dwellings,]

# see how the seven room dwellings and the eight room dwellings differ
mean(seven_room_dwellings_dat$tax)
mean(seven_room_dwellings_dat$crim)
mean(seven_room_dwellings_dat$ptratio)
mean(eight_room_dwellings_dat$tax)
mean(eight_room_dwellings_dat$crim)
mean(eight_room_dwellings_dat$ptratio)
