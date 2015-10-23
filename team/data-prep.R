#######
#
#  10/22/2015 UPDATE:  this is making a comeback!  now adds a 'duration' column, then deletes NAs
#                      but you should use some method/algorithm to collapse measurement columns
#                      and group into one row per Id before you do anything interesting
# 
#  Base data prep script for the Kaggle Rain project.  This will read the CSVs in and then 
#  save the as Rdata files locally, then only re-load CSVs when the Rdata files are not there.
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
    cat("loading train from Rdata file\n")
    load("../train.Rdata")
  } else {
    cat("loading train from CSV\n")
    train <- fread("../train.csv")
    
    #sort by Id then chronological
    setkey(train, Id, minutes_past)
    
    #add duration of measurements
    train <- train[ , duration := duration(minutes_past), Id]
    
    #remove the obs that are all NA for measurement columns
    na_obs <- train %>% 
      select( starts_with("Ref"), starts_with("Rho"), starts_with("Zdr"), starts_with("Kdp")) %>%
      .[, is.na(.SD)] %>% 
      rowSums() == 20
    train <- train[ ! na_obs, ] 
    
    #add a scaled duration of measurements after removing NAs
    train <- train[ , duration.scaled := durationscaled(duration), Id]
    
    save(train, file="../train.Rdata")
  }
}

#create variable lists for use in later function calls
vars.refcols <- c("Ref", "Ref_5x5_10th", "Ref_5x5_50th", "Ref_5x5_90th",
                  "RefComposite", "RefComposite_5x5_10th", "RefComposite_5x5_50th", "RefComposite_5x5_90th")
vars.rhocols <- c("RhoHV", "RhoHV_5x5_10th", "RhoHV_5x5_50th", "RhoHV_5x5_90th")
vars.zdrcols <- c("Zdr", "Zdr_5x5_10th", "Zdr_5x5_50th", "Zdr_5x5_90th")
vars.kdpcols <- c("Kdp", "Kdp_5x5_10th", "Kdp_5x5_50th", "Kdp_5x5_90th")

#create random sample of 1000 Ids (and then all associated rows) for testing ops
if (!exists("train.sample1000")) {
  set.seed(498)
  Ids.sample1000 <- sample(unique(train$Id), 1000)
  train.sample1000 <- train[Id==Ids.sample1000,]
}

#load test
if (!exists("test")) {
  if (file.exists("../test.Rdata")) {
    cat("loading test from Rdata file\n")
    load("../test.Rdata")
  } else {
    cat("loading test from CSV\n")
    test <- fread("../test.csv")
    save(test, file="../test.Rdata")
  }
}

