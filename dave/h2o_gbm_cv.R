library(data.table)
library(fastmatch)
library(zoo)
library(Metrics)
library(dplyr)
library(h2o)
h2o.init(
    nthreads=-1,            ## -1: use all available threads
    max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

source( "../team/rain_utils.R")

if (! tcheck.print) cat ("Silent Mode ... for Verbose set tcheck.print <- TRUE\n")
######################################### params
# set rdata_file outside this script to a raw csv read (don't use data-prep)
#rdata_file <- "../train_agg2.RData" # 
#rdata_file <- "../train_agg2_10pct.RData" # 


#def_cv_frac_trn <- 1   ## set this to 1 for no cross validation (maximize training for submission)
def_cv_frac_trn <- 0.7  # standard 70/30 split for C
def_create_submission <- FALSE
def_rain_thresh <- 65
def_cs <- c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec")
def_run_id <- format(Sys.time(), "gbm_%Y_%m_%d_%H%M%S")
def_ntrees <- 50
def_csv_dir <- 'csv_out'

if ( exists("set_cs") ) { cs <- set_cs } else { cs <- def_cs }

seed <- ifelse ( exists("set_seed"), set_seed, 99 )
ntrees <- ifelse ( exists("set_ntrees"), set_ntrees, def_ntrees )
rain_thresh <- ifelse( exists("set_rain_thresh"), set_rain_thresh, def_rain_thresh)

if (! exists( "cv_frac_trn")) cv_frac_trn <- def_cv_frac_trn
if (! exists( "create_submission")) create_submission <- def_create_submission
if (  exists( "chg_mpalmer"))  mpalmer <- chg_mpalmer
if (! exists( "run_id") ) run_id <- def_run_id
if (! exists( "csv_dir")) csv_dir <- def_csv_dir
run_id <- gsub("^.*/", "", run_id)   #legacy runs had the directory embedded

####################################################
cat ( sprintf( "h2o_gbm_cv.R runtime %s (seed = %d)\nntrees=%d\n", format(Sys.time()), seed, ntrees) )
cat ("Training data will be loaded from ", rdata_file, "\n")
cat ("Run ID = ", run_id, "\n")

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
tr$target <- log1p( tr$Expected)

df_trn <- paste0( run_id, ".trn")
h2o.trn <- as.h2o( tr, destination_frame = df_trn)    ; tcheck( desc='convert to h2o df')

true_y <- tr$Expected
rm(tr, train_agg)
gc()
h2o.mod<-h2o.gbm(x=cs, y="target"
                        ,training_frame=h2o.trn
                        ,model_id="gbm1"
                , ntrees=ntrees
                , max_depth=8
#                 , stopping_metric = "MSE"
#                 , stopping_tolerance = .0001
                , nfolds=0 )  ;tcheck(desc='build h2o model')
print(h2o.mod)
print(h2o.varimp(h2o.mod))

predictions<-as.data.frame(h2o.predict(h2o.mod, h2o.trn))   ;tcheck(desc='predict on scrubbed CV training data')

mae_scrub_trn <- mae( expm1(predictions$predict), true_y )
cat( "MAE for model data =", mae_scrub_trn, "\n")

#reload train to look at fit for the training dataset  (TODO: should probably roll some of this into a function)
load( rdata_file )                ; tcheck( desc='reload data')

get_predictions <- function( dt, dfname = "h2o.cv" ) {
    cv <- dt[ ! is.na(Ref), ] %>% as.data.frame()
    h2o.cv <- as.h2o( cv, destination_frame = dfname) 
    predictions<-as.data.frame(h2o.predict(h2o.mod, h2o.cv))
    cv$yhat <- expm1(predictions$predict)
    if ( any(  grepl ("Expected", colnames(cv)))) {
        cv %>% select( Id, Expected = yhat, y = Expected )
    } else {
        cv %>% select( Id, Expected = yhat )
    }
}

res <- get_predictions( train_agg[ cv_ix_trn, ], dfname = "cv_train" )   ;tcheck( desc='predict logvals on cv_train')

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254
mae_cv_trn <- mae( res$y, res$Expected )
cat( "MAE for CV train data =", mae_cv_trn, "\n")

if (cv_frac_trn < 1) { 
    res <- get_predictions( train_agg[  !cv_ix_trn, ], dfname = "cv_test" )   ;tcheck( desc='predict logvals on cv_test')
    
    #convert expected values to 0.01in values
    res$Expected <- round(res$Expected / 0.254) * 0.254
    mae_cv_test <- mae( res$y, res$Expected )
    cat( "MAE for CV test data =", mae_cv_test, "\n")
    
    if (create_submission) {
        csv <- sprintf( "%s/%s-cvtest.csv", csv_dir, run_id)
        write.csv( res, csv, row.names = FALSE)
    }
    
#     mp_baseline<-fread("KS_mpalmer-train.csv")  %>% rename( mp=Expected)
#     res <- res %>% left_join( mp_baseline, by="Id") %>% rename( yhat_gbm=Expected)
#     
#     px <- seq(0,1,0.05)
#     mae_gbm.mp <- numeric( length(px))
#     for ( i in 1:length(px)) mae_gbm.mp[i] <- mae( res$y, blend( res$yhat_gbm, res$mp, px[i]))
#     blend.gbm.mp <- data.frame( px, mae_gbm.mp)
#     blend.gbm.mp  %>% ggvis( ~px, ~mae_gbm.mp) %>% layer_points()
    
    
} else { 
    mae_cv_test <- NA
}

######################
mae_test <- NA
if ( create_submission) {
    cat ("... creating submission file using ", rtest_file, "\n")
    load( rtest_file )
    
    test_NAs <- test_agg[  is.na(Ref), .(Id = Id, Expected = train_NA)]

    res <- get_predictions( test_agg, "df_test" )   ;tcheck( desc='predict logvals on test')
    res$Expected <- round(res$Expected / 0.254) * 0.254

    res <- res %>% 
        bind_rows( test_NAs) %>%
        arrange( Id )
    
    csv <- sprintf( "%s/%s.csv", csv_dir, run_id)
    write.csv(res, csv, row.names = FALSE)    ; tcheck( desc='write submission file')
}

time_df <- get_tcheck()
print( time_df )
print( sum( time_df$delta ))

rm(run_id)
