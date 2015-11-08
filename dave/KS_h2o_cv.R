library(data.table)
library(fastmatch)
library(zoo)
library(Metrics)
library(dplyr)
library(h2o)

source( "../team/rain_utils.R")

#TODO:  Doesn't handle all the parameters below (from gbm_cv.R)
rdata_file <- 'train_10pct.RData'

if (! tcheck.print) cat ("Silent Mode ... for Verbose set tcheck.print <- TRUE\n")
######################################### params
#rdata_file <- "../train_agg.RData" # 
#rdata_file <- "../train_agg_10pct.RData" # 


#def_cv_frac_trn <- 1   ## set this to 1 for no cross validation (maximize training for submission)
def_cv_frac_trn <- 0.7  # standard 70/30 split for C
def_create_submission <- FALSE
def_submission_file <- "gbm_cv.csv"    #should have used this for the flag, but too late now
def_rain_thresh <- 65
def_cs <- c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec")

if ( exists("set_cs") ) { cs <- set_cs } else { cs <- def_cs }

seed <- ifelse ( exists("set_seed"), set_seed, 1999 )
rain_thresh <- ifelse( exists("set_rain_thresh"), set_rain_thresh, def_rain_thresh)
submission_file <- ifelse( exists("set_submission_file"), set_submission_file, def_submission_file)

if (! exists( "cv_frac_trn")) cv_frac_trn <- def_cv_frac_trn
if (! exists( "create_submission")) create_submission <- def_create_submission
if (  exists( "chg_mpalmer"))  mpalmer <- chg_mpalmer


####################################################
cat ( sprintf( "gbm_cv.R runtime %s (seed = %d)\n", format(Sys.time()), seed) )
cat ("Training data will be loaded from ", rdata_file, "\n")

# H2ORF cleaned, cut 69 pur vars 3 n 3
# last run 2 days ago by Bojan Tunguz in How Much Did It Rain? II 
# L forked from H2ORF cleaned, cut 70 pur vars 3 n by DavidLucas | +0 / -0 / ~5
# 
# 0 Voters 4 Forks 207 Views 1156.71s R
# Version 3/3
# Share
# Fork Script
# rfv3cn3.csv
# Submit to How Much Did It Rain? II

############################################
## Use H2O to create a random forest
##  against the entire data set in 
##  just a couple minutes
##
## This is a starter script, using defaults
##  so it can be improved. And RF may not be
##  the best algorithm for this problem. But
##  the script shows that it can be done in R
##  fairly quickly, in fact. And it will scale
##  well to adding many more columns.
##
## To better fit MAE, the log of the 
##  target has been used: log1p/expm1
##
## The final step blends with the Marshall-Palmer
##  benchmark 50/50.
##
## The API for h2o.randomForest is shown 
##  at the bottom of the script
###########################################

h2o.init(nthreads=-1)

tcheck(0)

## use data table to only read the Estimated, Ref, and Id fields
print(paste("reading training file:",Sys.time()))
#train<-fread("../input/train.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19, 24))
load(rdata_file)
train <- train %>% select( c(1,2,3,4,6,7,8,10,11,16,18, 19, 24))   ; tcheck(desc="load training data")

if (cv_frac_trn < 1) {
    ids <- unique(train$Id)
    set.seed( seed )
    cv_ids_trn <- sample( ids, round(cv_frac_trn * length(ids)) )
    cv_ix_trn <- train$Id %in% cv_ids_trn
    train <- train[ cv_ix_trn,  ]                            ;tcheck( desc='partition cv_train')
}

#Cut off outliers of Expected >= 69
train <- subset(train, Expected < 69)

# summary(train)

#Cut off Ref values < 0
train$Ref_5x5_50th[which(train$Ref_5x5_50th < 0)] <- NA
train$Ref_5x5_90th[which(train$Ref_5x5_90th < 0)] <- NA
train$RefComposite[which(train$RefComposite < 0)] <- NA
train$RefComposite_5x5_50th[which(train$RefComposite_5x5_50th < 0)] <- NA
train$RefComposite_5x5_90th[which(train$RefComposite_5x5_50th < 0)] <- NA
train$Ref[which(train$Ref < 0)] <- NA                       ;tcheck( desc='set cut off vals')

# cor(train, use = "pairwise.complete.obs")
# summary(train)

trainHex<-as.h2o(train[,.(
    dist   = mean(radardist_km, na.rm = T),
    refArea5   = mean(Ref_5x5_50th, na.rm = T),
    refArea9  = mean(Ref_5x5_90th, na.rm = T),
    meanRefcomp = mean(RefComposite,na.rm=T),
    meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
    meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
    zdr   = mean(Zdr, na.rm = T),
    zdr5   = mean(Zdr_5x5_50th, na.rm = T),
    zdr9   = mean(Zdr_5x5_90th, na.rm = T),
    target = log1p(mean(Expected)),
    meanRef = mean(Ref,na.rm=T),
    sumRef = sum(Ref,na.rm=T),
    records = .N,
    naCounts = sum(is.na(Ref))
),Id][records>naCounts,],destination_frame="train.hex")    ;tcheck( desc='create summary data frame')

# summary(trainHex)

rfHex<-h2o.randomForest(x=c("dist", "refArea5", "refArea9", "meanRefcomp","meanRefcomp5","meanRefcomp9", "zdr",
                            "zdr5", "zdr9", "meanRef","sumRef", "records","naCounts"
),
y="target",training_frame=trainHex,model_id="rfStarter.hex", ntrees=500, sample_rate = 0.7) ;tcheck('random forest')
print(rfHex)
h2o.varimp(rfHex)
rm(train)

load(rdata_file)
test <- train[ ! cv_ix_trn,  ] %>% select( c(1,2,3,4,6,7,8,10,11,16,18, 19, 24))   ; tcheck(desc="reload training data as CV test")
rm(train)

#test<-fread("../test.csv",select=c(1,2,3,4,6,7,8,10,11,16,18, 19))

#Cut off Ref values < 0
test$Ref_5x5_50th[which(test$Ref_5x5_50th < 0)] <- NA
test$Ref_5x5_90th[which(test$Ref_5x5_90th < 0)] <- NA
test$RefComposite[which(test$RefComposite < 0)] <- NA
test$RefComposite_5x5_50th[which(test$RefComposite_5x5_50th < 0)] <- NA
test$RefComposite_5x5_90th[which(test$RefComposite_5x5_90th < 0)] <- NA
test$Ref[which(test$Ref < 0)] <- NA


testHex<-as.h2o(test[,.(
    dist   = mean(radardist_km, na.rm = T),
    refArea5   = mean(Ref_5x5_50th, na.rm = T),
    refArea9  = mean(Ref_5x5_90th, na.rm = T),
    meanRefcomp = mean(RefComposite,na.rm=T),
    meanRefcomp5 = mean(RefComposite_5x5_50th,na.rm=T),
    meanRefcomp9 = mean(RefComposite_5x5_90th,na.rm=T),
    zdr   = mean(Zdr, na.rm = T),
    zdr5   = mean(Zdr_5x5_50th, na.rm = T),
    zdr9   = mean(Zdr_5x5_90th, na.rm = T),
    
    meanRef = mean(Ref,na.rm=T),
    sumRef = sum(Ref,na.rm=T),
    records = .N,
    naCounts = sum(is.na(Ref))
),Id],destination_frame="test.hex")

#summary(testHex)

submission<-fread("../input/sample_solution.csv")
predictions<-as.data.frame(h2o.predict(rfHex,testHex))

res <- test[, .(y=Expected, yhat := predictions$predict)]
mae_h2o <- mae( res$y, res$yhat)
#submission$Expected<- 0.75 * expm1(predictions$predict) + 0.25 * submission$Expected

#convert expected values to 0.01in values
res$yhat2 <- round(res$yhat / 0.254) * 0.254
mae_h2o2 <- mae( res$y, res$yhat2)

print(mae_h2o)
print(mae_h2o2)
#summary(submission)
#write.csv(submission,"rfv3cn3.csv",row.names=F)


####################################################################################
## Appendix: h2o.randomForest API
####################################################################################
##h2o.randomForest(x, y, training_frame, model_id, validation_frame, checkpoint,
##  mtries = -1, sample_rate = 0.632, build_tree_one_node = FALSE,
##  ntrees = 50, max_depth = 20, min_rows = 1, nbins = 20,
##  nbins_cats = 1024, binomial_double_trees = FALSE,
##  balance_classes = FALSE, max_after_balance_size = 5, seed,
##  offset_column = NULL, weights_column = NULL, nfolds = 0,
##  fold_column = NULL, fold_assignment = c("AUTO", "Random", "Modulo"),
##  keep_cross_validation_predictions = FALSE, ...)
## Arguments

## x	
## A vector containing the names or indices of the predictor variables to use in building the GBM model.

## y	
## The name or index of the response variable. If the data does not contain a header, this is the column index number starting at 1, and increasing from left to right. (The response must be either an integer or a categorical variable).

## training_frame	
## An H2OFrame object containing the variables in the model.

## model_id	
## (Optional) The unique id assigned to the resulting model. If none is given, an id will automatically be generated.

## validation_frame	
## An H2OFrame object containing the variables in the model.

## checkpoint	
## "Model checkpoint (either key or H2ODeepLearningModel) to resume training with."

## mtries	
## Number of variables randomly sampled as candidates at each split. If set to -1, defaults to sqrtp for classification, and p/3 for regression, where p is the number of predictors.

## sample_rate	
## Sample rate, from 0 to 1.0.   (edit: row sampling, per tree)

## build_tree_one_node	
## Run on one node only; no network overhead but fewer cpus used. Suitable for small datasets.

## ntrees	
## A nonnegative integer that determines the number of trees to grow.

## max_depth	
## Maximum depth to grow the tree.

## min_rows	
## Minimum number of rows to assign to teminal nodes.

## nbins	
## For numerical columns (real/int), build a histogram of this many bins, then split at the best point.

## nbins_cats	
## For categorical columns (enum), build a histogram of this many bins, then split at the best point. Higher values can lead to more overfitting.

## binomial_double_trees	
## For binary classification: Build 2x as many trees (one per class) - can lead to higher accuracy.

## balance_classes	
## logical, indicates whether or not to balance training data class counts via over/under-sampling (for imbalanced data)

## max_after_balance_size	
## Maximum relative size of the training data after balancing class counts (can be less than 1.0)

## seed	
## Seed for random numbers (affects sampling) - Note: only reproducible when running single threaded

## offset_column	
## Specify the offset column.

## weights_column	
## Specify the weights column.

## nfolds	
## (Optional) Number of folds for cross-validation. If nfolds >= 2, then validation must remain empty.

## fold_column	
## (Optional) Column with cross-validation fold index assignment per observation

## fold_assignment	
## Cross-validation fold assignment scheme, if fold_column is not specified Must be "AUTO", "Random" or "Modulo"

## keep_cross_validation_predictions	
## Whether to keep the predictions of the cross-validation models
