
setwd("/Users/goutham/Development/statistical_data_mining/homework_1")

library("DAAG")
library("lattice")
library("MASS")
library("geneplotter")
library("gdata")     
library(Hmisc)

tbl <- read.csv("cereal.csv")

tbl_temp <- tbl

# getting rid of the columns with non-numerical data for histogram plotting
numeric_table_dat <- tbl[,-(1:3)]

# eliminate outliers with negative values
eliminated_outliers <- numeric_table_dat
outliers <- which(numeric_table_dat$sugars< 0 | numeric_table_dat$potass< 0) # which command (dataset rows where weight > 200)
eliminated_outliers <- numeric_table_dat[-outliers,]
hist.data.frame(eliminated_outliers) # plot data after eliminating ourliers

# adding back the columns with non-numerical data
final_dat_outliers <- which(tbl$sugars< 0 | tbl$potass< 0) # which command (dataset rows where weight > 200)
tbl_temp <- tbl_temp[-final_dat_outliers,]
saved_dataset <- eliminated_outliers

# saving data and saving dataset
saved_dataset=cbind(saved_dataset, tbl_temp$name)
saved_dataset=cbind(saved_dataset, tbl_temp$mfr)
saved_dataset=cbind(saved_dataset, tbl_temp$type)
save(saved_dataset, file = "cleaned_dataset.RData")

