# Objective: USe RhoHV and ZdR to describe types of rainfall.
library(data.table)
library(lattice)
library(dplyr)
#library(memisc) 

source("../team/data-prep.R")
source("../team/rain_utils.R")

nested_ifelse <- function(x)
  ifelse (x < 0.2, "Out of Range (Low)",
    ifelse (x >= 0.2 & x < 0.85,   "Non-Meteorological",
            ifelse(x >= 0.85 & x < 0.97, "Non-Uniform", 
                   ifelse(x >= 0.97 & x <= 1.05, "Uniform",
                                 ifelse(x > 1.05, "Out of Range (High)",
                   "Not Classified" )))))

train$type <- nested_ifelse(train$RhoHV)

# Count number of records across dimensionts
# Non-Meteorological: 270,984 (1.97%)
# Non-Uniform:        900,732 (6.54%)
# Uniform:            3,317,982 (24.10%)
# Out of Range (High) 445,218 (3.23%)

data.frame(table(train$type))

# Create Trellis accross type dimension

trainrnd <- sample_frac(train, 0.1, replace=TRUE)

densityplot(~Expected|type,data=trainrnd,
            xlab="Expected (mm)",
            main="Expected (measured) precipitation,\nby precip type")

summary(subset(train, type == "Non-Meteorological")$Expected)
summary(subset(train, type == "Non-Uniform")$Expected)
summary(subset(train, type == "Uniform")$Expected)
summary(subset(train, type == "Out of Range (High)")$Expected)

