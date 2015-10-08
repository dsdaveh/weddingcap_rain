#######
#
#  Base data prep script for the Kaggle Rain project.  This will read the CSVs in and then 
#  save the as Rdata files locally, then only re-load CSVs when the Rdata files are not there.
#
#  You can conditionally call this from other scripts you write in your sub-directory by 
#  including this at the top of your script:
#         #load and prep the data if this hasn't been done
#         if (!exists("train")) source("../team/data-prep.R", echo=FALSE, print.eval=FALSE)
#
#  Output data frames:
#  - train:  training data set with NAs filtered out
#  - test:  test data set with NAs preserved
#  - train.sample1000:  subsample of 1000 random Ids and all associated rows, useful for 
#                       running tests that return more quickly

library(data.table)
library(dplyr)

# load train
if (!exists("train")) {
  if (file.exists("../train.Rdata")) {
    cat("loading train from Rdata file")
    load("../train.Rdata")
  } else {
    cat("loading train from CSV")
    train <- fread("../train.csv")
    
    #remove the obs that are all NA for measurement columns
    na_obs <- train %>% 
      select( starts_with("Ref"), starts_with("Rho"), starts_with("Zdr"), starts_with("Kdp")) %>%
      .[, is.na(.SD)] %>% 
      rowSums() == 20
    train <- train[ ! na_obs, ] 
    
    save(train, file="../train.Rdata")
  }
}

#create random sample of 1000 Ids (and then all associated rows) for testing ops
if (!exists("train.sample1000")) {
  set.seed(498)
  Ids.sample1000 <- sample(unique(train$Id), 1000)
  train.sample1000 <- train[Id==Ids.sample1000,]
}

#load test
if (!exists("test")) {
  if (file.exists("../test.Rdata")) {
    cat("loading test from Rdata file")
    load("../test.Rdata")
  } else {
    cat("loading test from CSV")
    test <- fread("../test.csv")
    save(test, file="../test.Rdata")
  }
}

