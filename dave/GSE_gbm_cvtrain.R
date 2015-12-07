######
##  This is a true CV model ... I was mis-using the term before.  I will only produce CV results
##  for a training set.   It's used to improve the parameters.  It will output a -cvtrain.csv file
##  which can be used for a Superlearner
## 
library(data.table)
library(fastmatch)
library(zoo)
library(xgboost)
library(Metrics)
library(dplyr)

source( "../team/rain_utils.R")
mae_xgb <- mae_cv_trn <- mae_cv_test <- NA

if (! tcheck.print) cat ("Silent Mode ... for Verbose set tcheck.print <- TRUE\n")
######################################### params
# set rdata_file outside this script to a raw csv read (don't use data-prep)
#rdata_file <- "../train_agg.RData" # 
#rdata_file <- "../train_agg_10pct.RData" # 


#def_cv_frac_trn <- 1   ## set this to 1 for no cross validation (maximize training for submission)
def_cv_frac_trn <- 0.7  # standard 70/30 split for C
def_kfold <- 5
def_create_submission <- FALSE
def_rain_thresh <- 65
def_rain_thresh_lower <- 0
def_cs <- c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec")
def_run_id <- format(Sys.time(), "%Y_%m_%d_%H%M%S")

if ( exists("set_cs") ) { cs <- set_cs } else { cs <- def_cs }

seed <- ifelse ( exists("set_seed"), set_seed, 1999 )
rain_thresh <- ifelse( exists("set_rain_thresh"), set_rain_thresh, def_rain_thresh)
rain_thresh_lower <- ifelse( exists("set_rain_thresh_lower"), set_rain_thresh_lower, def_rain_thresh_lower)
kfold <- ifelse( exists("set_kfold"), set_kfold, def_kfold)

if (! exists( "cv_frac_trn")) cv_frac_trn <- def_cv_frac_trn
if (! exists( "create_submission")) create_submission <- def_create_submission
if (  exists( "chg_mpalmer"))  mpalmer <- chg_mpalmer
if (! exists( "run_id") ) run_id <- def_run_id


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
tr <- tr[ !is.na(Ref) ]
tr <- tr[round(Expected, 4) %fin% valid_vals, ]
tr_ids <- tr$Id

y<- log1p( tr$Expected )
tr<-as.data.frame(tr)
res <- tr %>% select( Id, Expected )
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

xgb.out  <- xgb.cv(params = param0, data = xgtrain 
                   , nfold = kfold, nrounds =1955
                   , prediction = TRUE, verbose = 0 )          ;tcheck( desc='xgb.train cv_train')


csv <- sprintf( "%s-cvtrain.csv", run_id)
write.csv( data.frame( Id = tr_ids, ln1py = xgb.out$pred)
           , csv, row.names = FALSE)

time_df <- get_tcheck()
print( time_df )
print( sum( time_df$delta ))

