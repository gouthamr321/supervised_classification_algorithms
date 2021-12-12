rm(list=ls())

setwd("/Users/goutham/Development/statistical_data_mining_projects")


# install some packages
#install.packages("DAAG")
#install.packages("lattice")
#install.packages("MASS")

#install.packages('bitops') 


library("DAAG")
library("lattice")
library("MASS")
library("geneplotter")

?possum
dim(possum)
names(possum)


fossum <-subset(possum, sex == "f")
write.table(fossum, file= "fem_possum_data", sep="\t", col.names=names(fossum))
#temp <- read.delim("fem_possum_data.txt", sep = "\t", header = True)

# plot histograms

par(mfrow = c(1,4))
attach(fossum)
hist(totlngth)
saveeps("density_plots")

# questions: what is this <- notation-- equivelent to assignment opperator ?




