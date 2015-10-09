#######
#
#  Base data prep script for the Kaggle Rain project.  This will read the CSVs in and then 
#  save the as Rdata files locally, then only re-load CSVs when the Rdata files are not there.
#
#  interative version breaks the process down into multiple chunks  in case the original version fails 
#  due to lack of memory (controlled by variable "chunks" default = 5)
#
#  You can call this from other scripts you write in your sub-directory by 
#  including this at the top of your script, and it will load data if necessary:
#         #load and prep the data if this hasn't been done
#         source("../team/data-prep.R", echo=FALSE, print.eval=FALSE)
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
    cat("loading train from CSV...\n")
    if (file.exists("tmp_train_full.RData")) {
      cat("using existing tmp file to start...\n")
      load("tmp_train_full.RData")
    } else {
      train <- fread("../train.csv")
      save( train, file="tmp_train_full.RData")
    }
    last <- nrow(train)
    
    train <- train[ -(1:nrow(train)), ]   #empty the data frame to start
    save( train, file="tmp_train_part.RData")
    
    if ( ! exists("chunks") ) chunks <- 5  # how many divisions to process the data in
    cut_pts <- seq( 1, last, round( last/chunks ))
    cut_pts[ length(cut_pts)] <- last
    
    for (i in 1:chunks ) {
      cat( sprintf("chunk %d...\n", i))
      load( file="tmp_train_full.RData")
      train <- train[ cut_pts[i]:cut_pts[i+1], ]
      
      na_obs <- train %>% 
        select( starts_with("Ref"), starts_with("Rho"), starts_with("Zdr"), starts_with("Kdp")) %>%
        .[, is.na(.SD)] %>% 
        rowSums() == 20
      
      
      train_next <- train[ ! na_obs, ]
      load( file="tmp_train_part.RData")
      train <- rbind( train, train_next)
      save( train, file="tmp_train_part.RData")
      rm( train_next)
    }
    load( file="tmp_train_part.RData")
    
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

