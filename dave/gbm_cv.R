library(data.table)
library(fastmatch)
library(zoo)
library(xgboost)
library(Metrics)
library(dplyr)

source( "../team/rain_utils.R")

if (! tcheck.print) cat ("Silent Mode ... for Verbose set tcheck.print <- TRUE\n")
######################################### params
# set rdata_file outside this script to a raw csv read (don't use data-prep)
#rdata_file <- "../train_agg.RData" # 
#rdata_file <- "../train_agg_10pct.RData" # 


#def_cv_frac_trn <- 1   ## set this to 1 for no cross validation (maximize training for submission)
def_cv_frac_trn <- 0.7  # standard 70/30 split for C
def_create_submission <- FALSE
def_rain_thresh <- 0.65

seed <- ifelse ( exists("set_seed"), set_seed, 1999 )
rain_thresh <- ifelse( exists("set_rain_thresh"), set_rain_thresh, def_rain_thresh)

if (! exists( "cv_frac_trn")) cv_frac_trn <- def_cv_frac_trn
if (! exists( "create_submission")) create_submission <- def_create_submission
if (  exists( "chg_mpalmer"))  mpalmer <- chg_mpalmer


####################################################
cat ( sprintf( "gbm_cv.R runtime %s (seed = %d)\n", format(Sys.time()), seed) )
cat ("Training data will be loaded from ", rdata_file, "\n")

cv_txt <- ifelse( cv_frac_trn < 1, "", "NOT")
cs_txt <- ifelse( create_submission, "", "NOT")
cat( "CV Fraction for training =", cv_frac_trn, ".  CV testing will", cv_txt, "be performed.\n")
cat( "create_submission =", create_submission, ".  Submission file will", cs_txt, "be created.\n")
cat( "Measured Rain Threshold for training data is", rain_thresh, "\n")

if ( ! exists( "mp_func")) mp_func <- ref_to_mm
cat(" Marshal Palmer function printed below.  Map to mp_func to change this (eg mp_func <- ref_to_mm_kaggle )\n")
print( mp_func )

##########FUNCTIONS###########
#Fast %in%
`%fin%` <- function(x, lkup) {
    fmatch(x, lkup, nomatch = 0L) > 0L
}

##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

######Training Data############
tcheck(0)
cat("loading train from RData file ", rdata_file, "\n")
load( rdata_file )
setkey( train_agg, Id)
tcheck( desc='load data')
cv_ix_trn <- rep( TRUE, nrow(train_agg))

if (cv_frac_trn < 1) {
    ids <- unique(train_agg$Id)
    set.seed( seed )
    cv_ids_trn <- sample( ids, round(cv_frac_trn * length(ids)) )
    cv_ix_trn <- train_agg$Id %in% cv_ids_trn                                 ;tcheck( desc='partition cv_train')
}

tr <- train_agg[ cv_ix_trn, ]
train_NA <- tr[ ! is.na(Ref), median(Expected)]    #probably could just set this to 0

#scrub tr
tr <- tr[ Expected <= rain_thresh, ]
tr <- tr[round(Expected, 4) %fin% valid_vals, ]

cs <- c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec")
y<- log1p( tr$Expected )
tr<-as.data.frame(tr)
tr<-tr[,cs]
param0 <- list("objective"  = "reg:linear" 
               , "eval_metric" = "rmse"
               , "eta" = 0.007
               , "subsample" = 0.8
               , "min_child_weight" =10    
               , "max_depth" = 8
               , "nthreads" = 4
)
xgtrain = xgb.DMatrix(as.matrix(tr), label = y, missing = NA);    tcheck( desc='construct train matrix')
rm(tr, train_agg)
gc()

x.mod.t  <- xgb.train(params = param0, data = xgtrain , nrounds =1955)          ;tcheck( desc='xgb.train cv_train')

pr_trn  <- predict(x.mod.t,xgtrain)                                       ;tcheck( desc='predict logvals on scrubbed model data')
mae_xgb <- mae( expm1(pr_trn), expm1(y) )
cat( "MAE for model data =", mae_xgb_trn, "\n")

#reload train to look at fit for the training dataset  (TODO: should probably roll some of this into a function)
load( rdata_file )                ; tcheck( desc='reload data')

get_predictions <- function( dt ) {
    cv <- as.data.frame( dt[ ! is.na(Ref), ] )
    xgtest = xgb.DMatrix(as.matrix( cv[,cs ]), missing = NA) 
    pr  <- predict(x.mod.t,xgtest)       
    cv$xgb_prediction <- expm1(pr)
    cv %>% select( Id, Expected = xgb_prediction, y = Expected )
}

res <- get_predictions( train_agg[ cv_ix_trn, ] )   ;tcheck( desc='predict logvals on cv_train')

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254
mae_cv_trn <- mae( res$y, res$Expected )
cat( "MAE for CV train data =", mae_cv_trn, "\n")

if (cv_frac_trn < 1) {
    res <- get_predictions( train_agg[  !cv_ix_trn, ] )   ;tcheck( desc='predict logvals on cv_test')
    
    #convert expected values to 0.01in values
    res$Expected <- round(res$Expected / 0.254) * 0.254
    mae_cv_test <- mae( res$y, res$Expected )
    cat( "MAE for CV test data =", mae_cv_test, "\n")
}

######################
test_file <- "../test_agg.Rdata"
if ( create_submission) {
    cat ("... creating submission file using ", test_file, "\n")
    load( test_file )
    
    test_NAs <- test_agg[  is.na(Ref), .(Id = Id, Expected = train_NA)]

    te<-as.data.frame(test_agg[ ! is.na(Ref) ])
    xgtest = xgb.DMatrix(as.matrix(te[,cs ]), missing = NA)
    
    pr  <- predict(x.mod.t,xgtest)                     ;tcheck( desc='predict logvals on test dataset')
    te$xgb_prediction <- expm1(pr)

    res <- te %>% 
        select( Id, Expected = xgb_prediction ) %>%
        bind_rows( test_NAs) %>%
        arrange( Id )
    
    #convert expected values to 0.01in values
    res$Expected <- round(res$Expected / 0.254) * 0.254
    
    write.csv(res, "gbm_cv.csv", row.names = FALSE)    ; tcheck( desc='write submission file')
    
}

time_df <- get_tcheck()
print( time_df )
print( sum( time_df$delta ))
