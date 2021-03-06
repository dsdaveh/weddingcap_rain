library(data.table)
library(Metrics)
library(dplyr)
library(lattice)
library(rpart)
library(randomForest)

#library(corrplot)
library(seriation)

# References:
# https://github.com/joewie/probabilistic-rain-prediction

# References:
# https://github.com/joewie/probabilistic-rain-prediction
# 

# 13,765,201 records
# 24 Columns

setwd("/Users/carlkoster/Documents/GitHub Repositories/weddingcap_rain/team")
source("rain_utils.R")

# Import the training sets
# Loads the standard data set with team pre-processing done for you
# Some NAs are removed
source("../team/data-prep.R", echo=FALSE, print.eval=FALSE)
#source("/Users/carlkoster/Documents/weddingcap_rain/team/data-prep.R", echo=FALSE, print.eval=FALSE)

# There are only 2769088 records with complete cases.
# Removes any row with at least one NA
train.complete.cases <- train[complete.cases(train),]

# Import the entire training set
train_org <- fread("../train.csv", stringsAsFactors = FALSE) 

# Import the full training set
train_org <- fread("../train.csv", stringsAsFactors = FALSE) 

# Remove unreasonable Expected headvalues
train2 <- subset(train, Expected <= 50) 


# Summary statistics indicates that almost every field appears to 
# have measurements that are not logical. This indicates a malfunction
# of the radar or the measurement.
head(train)
summary(train)


# Histogram of the expected values
# Note that the distribution appears to be centered near zero
# The median is 1.02
# Should remove all Expected valiues that are less than zero
histogram(~ Expected, train, nint = 100)
histogram(~ log(Expected), train, nint = 100)

###########
# Density plot of Expected
###########

d <- density(train2$Expected)
plot(d, main="Kernel Density of Expected (mm/hr)")
#polygon(d, col="blue", border="red")


###########
# Correlation matrix
###########
cm1 <- cor(sample_n(train.complete.cases, 2000000))
pimage(cm1)

###########
# Random Forest
###########

# All predictors
frmla = Expected ~ minutes_past + radardist_km + Ref + Ref_5x5_10th + Ref_5x5_50th + Ref_5x5_90th + RefComposite + RefComposite_5x5_10th + RefComposite_5x5_50th + RefComposite_5x5_90th + RhoHV + RhoHV_5x5_10th + RhoHV_5x5_50th + RhoHV_5x5_90th + Zdr + Zdr_5x5_10th + Zdr_5x5_50th + Zdr_5x5_90th + Kdp + Kdp_5x5_10th + Kdp_5x5_50th + Kdp_5x5_90th
#fit = rpart(frmla, method="poisson", data=train)
# na.roughfix fixes missing values with the mean or mode of the missing variable.
#fit.rf.org <- randomForest(frmla, data=train_org, na.action=na.roughfix)
fit.rf.train <- randomForest(frmla, data=train, na.action=na.roughfix)
fit.rf.train2 <- randomForest(frmla, data=train2, na.action=na.roughfix)

# Trees
# Naive Bayes

###########
# PCA
###########

# Reference: https://www.youtube.com/watch?v=Heh7Nv4qimU
# https://onlinecourses.science.psu.edu/stat505/node/54

train.complete.cases.sample <- sample_n(train.complete.cases, 10000)
train.complete.cases.m <- as.matrix(train.complete.cases.sample)
train.complete.cases.m <- train.complete.cases.m[,2:23]

#arc.pca1 <- princomp((sample_n(train.complete.cases, 1000)), scores=TRUE, COR=TRUE)
arc.pca1 <- princomp(train.complete.cases.m, scores=TRUE, COR=TRUE)
summary(arc.pca1)

# Plot the scree plot
plot(arc.pca1)

# Print the biplot
biplot(arc.pca1, scale = 0)
# Print the loadings
arc.pca1$loadings

# print the scores (projections)
arc.pca1$scores