library(data.table)
library(fastmatch)
library(zoo)
library(xgboost)
library(Metrics)
library(dplyr)

source( "../team/rain_utils.R")

######################################### params
# set rdata_file outside this script to a raw csv read (don't use data-prep)
#rdata_file <- "train_full.RData" # 
#rdata_file <- "train_10pct.RData" # 


#def_cv_frac_trn <- 1   ## set this to 1 for no cross validation (maximize training for submission)
def_cv_frac_trn <- 0.7  # standard 70/30 split for CV

def_create_submission <- FALSE

if (! exists( "cv_frac_trn")) cv_frac_trn <- def_cv_frac_trn
if (! exists( "create_submission")) create_submission <- def_create_submission
if (  exists( "chg_mpalmer"))  mpalmer <- chg_mpalmer

####################################################
cat ("KS_gbm_cv.R runtime ", format(Sys.time()), "\n")
cat ("Training data will be loaded from ", rdata_file, "\n")

cv_txt <- ifelse( cv_frac_trn < 1, "", "NOT")
cs_txt <- ifelse( create_submission, "", "NOT")
cat( "CV Fraction for training =", cv_frac_trn, ".  CV testing will", cv_txt, "be performed.\n")
cat( "create_submission =", create_submission, ".  Submission file will", cs_txt, "be created.\n")

if ( exists( "chg_mpalmer"))  cat ("mpalmer has been changed from default")
#Creating a semi random Ref profile to get the mpalmer scalar that we can use to identify which mpalmer was used
# DO NOT CHANGE these values, since it will invalidate any save output.
mp_sig <- data.frame( Ref = c( NA, 5, 10, NA, NA, NA, 20, 5, NA, NA),
                      mph = 4 * 1:10 )
cat(" MPalmer signature = ", mpalmer( mp_sig$Ref, mp_sig$mph ), "\n")

##########FUNCTIONS###########
#Fast %in%
`%fin%` <- function(x, lkup) {
    fmatch(x, lkup, nomatch = 0L) > 0L
}

#Get the time differences between each measure
time_difference <- function(times, num_per_segment = 60) {
    n <- length(times)
    valid_time <- vector(mode="numeric", length = n)
    valid_time[1] <- times[1]
    valid_time[-1] <- diff(times, 1)
    valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
    valid_time <- valid_time / num_per_segment
    valid_time
}

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
    ((10**(dbz/10))/200) ** 0.625
}

##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

######Training Data############
tcheck(0)
cat("loading train from RData file ", rdata_file, "\n")
load( rdata_file )
zzz <- tcheck( desc='load data'); print(zzz)

train_NA <- train[ , .( 
    refNA = all(is.na(Ref)), Expected = max(Expected)), Id][ refNA == TRUE, median(Expected)]


if (cv_frac_trn < 1) {
    ids <- unique(train$Id)
    set.seed( 1999)
    cv_ids_trn <- sample( ids, round(cv_frac_trn * length(ids)) )
    cv_ix_trn <- train$Id %in% cv_ids_trn
    train <- train[ cv_ix_trn,  ] ;zzz <- tcheck( desc='partition cv_train'); print(zzz)
}

tr_raw <- train %>%  select(
    Id, 
    minutes_past, 
    radardist_km,
    Ref, 
    RefComposite, 
    Expected)

tr_raw <- subset(tr_raw, Expected <= 65)
tr_raw <- tr_raw[round(Expected, 4) %fin% valid_vals]
tr_raw$dt <- time_difference(tr_raw$minutes_past)
tr_raw$mp <- marshall_palmer(tr_raw$Ref)

#Collapse to one record per Id
tr <- tr_raw[, .(
    target = log1p(mean(Expected, na.rm = T)),
    ref = mean(dt * Ref, na.rm = T),
    ref1 = mean(dt * RefComposite, na.rm = T),
    mp = mpalmer(Ref, minutes_past), #mp = sum(dt * mp, na.rm = T),  # #replaced dah
    rd = mean(radardist_km, na.rm = T),
    records = .N,
    naCounts = sum(is.na(Ref))
), Id]


print("training model...")
cs <- c("ref", "ref1",   "mp", "rd", "records")
y<-tr$target
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
xgtrain = xgb.DMatrix(as.matrix(tr), label = y, missing = NA);zzz <- tcheck( desc='construct train matrix'); print(zzz)
rm(tr,tr_raw,train)
gc()

x.mod.t  <- xgb.train(params = param0, data = xgtrain , nrounds =1955);zzz <- tcheck( desc='xgb.train cv_train'); print(zzz)

pr_trn  <- predict(x.mod.t,xgtrain);zzz <- tcheck( desc='predict logvals on scrubbed model data'); print(zzz)
mae_xgb_trn <- mae( expm1(pr_trn), expm1(y) )
cat( "MAE for model data =", mae_xgb_trn, "\n")

#reload train to look at fit for the training dataset  (TODO: should probably roll some of this into a function)
load( rdata_file ) ; zzz <- tcheck( desc='reload data'); print(zzz)

te_raw<- train[ cv_ix_trn,  ] %>%  select(
    Id, 
    minutes_past, 
    radardist_km,
    Ref, 
    RefComposite, 
    Expected)

te_raw$dt <- time_difference(te_raw$minutes_past)
te_raw$mp <- marshall_palmer(te_raw$Ref)

tecv <- te_raw[, .(
    noRef = all( is.na(Ref)),
    ref = mean(dt * Ref, na.rm = T),
    ref1 = mean(dt * RefComposite, na.rm = T),
    mp = mpalmer(Ref, minutes_past),  #mp = sum(dt * mp, na.rm = T), #replaced dah
    rd = mean(radardist_km),
    records = .N,
    y = max(Expected)
),Id][ noRef == FALSE , ]
tecv<-as.data.frame(tecv)
tcheck( desc='prepare cv_train data'); print(zzz)

xgtest = xgb.DMatrix(as.matrix(tecv[,cs ]), missing = NA);zzz <- tcheck( desc='construct cv_train matrix'); print(zzz)

pr  <- predict(x.mod.t,xgtest);zzz <- tcheck( desc='predict logvals on cv_train'); print(zzz)
tecv$xgb_prediction <- expm1(pr)

res <- tecv %>% select( Id, Expected = xgb_prediction )

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254
mae_xgb <- mae( tecv$y, res$Expected )
cat( "MAE for CV train data =", mae_xgb, "\n")

if (cv_frac_trn < 1) {
    
    te_raw<- train[ ! cv_ix_trn,  ] %>%  select(
        Id, 
        minutes_past, 
        radardist_km,
        Ref, 
        RefComposite, 
        Expected)
    
    te_raw$dt <- time_difference(te_raw$minutes_past)
    te_raw$mp <- marshall_palmer(te_raw$Ref)
    
    tecv <- te_raw[, .(
        noRef = all( is.na(Ref)),
        ref = mean(dt * Ref, na.rm = T),
        ref1 = mean(dt * RefComposite, na.rm = T),
        mp = mpalmer(Ref, minutes_past),  #mp = sum(dt * mp, na.rm = T), #replaced dah
        rd = mean(radardist_km),
        records = .N,
        y = max(Expected)
    ),Id][ noRef == FALSE , ]
    tecv<-as.data.frame(tecv)
    tcheck( desc='prepare cv_test data'); print(zzz)
    
    xgtest = xgb.DMatrix(as.matrix(tecv[,cs ]), missing = NA);zzz <- tcheck( desc='construct cv_test matrix'); print(zzz)
    
    pr  <- predict(x.mod.t,xgtest);zzz <- tcheck( desc='predict logvals on cv_test'); print(zzz)
    tecv$xgb_prediction <- expm1(pr)

    res <- tecv %>% select( Id, Expected = xgb_prediction )
    
    #convert expected values to 0.01in values
    res$Expected <- round(res$Expected / 0.254) * 0.254
    mae_xgb <- mae( tecv$y, res$Expected )
    cat( "MAE for CV test data =", mae_xgb, "\n")
}

######################
test_file <- "../test.Rdata"
if ( create_submission) {
    cat ("... creating submission file using ", test_file, "\n")
    load("../test.Rdata")
    
    test_NAs <- test[ , .( refNA = all(is.na(Ref))), Id][ refNA == TRUE, .(Id = Id, Expected = train_NA)]

    te_raw <- test %>%  select(
        Id, 
        minutes_past, 
        radardist_km,
        Ref, 
        RefComposite
        )
    
    te_raw$dt <- time_difference(te_raw$minutes_past)
    te_raw$mp <- marshall_palmer(te_raw$Ref)
    
    te <- te_raw[, .(
        noRef = all( is.na(Ref)),
        ref = mean(dt * Ref, na.rm = T),
        ref1 = mean(dt * RefComposite, na.rm = T),
        mp = mpalmer(Ref, minutes_past),  #replaced dah#mp = sum(dt * mp, na.rm = T), #
        rd = mean(radardist_km),
        records = .N
    ),Id][ noRef == FALSE , ]
    te<-as.data.frame(te)
    tcheck( desc='prepare kaggle test data'); print(zzz)
    
    xgtest = xgb.DMatrix(as.matrix(te[,cs ]), missing = NA);zzz <- tcheck( desc='construct test matrix'); print(zzz)
    
    pr  <- predict(x.mod.t,xgtest);zzz <- tcheck( desc='predict logvals on test'); print(zzz)
    te$xgb_prediction <- expm1(pr)

    res <- te %>% 
        select( Id, Expected = xgb_prediction ) %>%
        bind_rows( test_NAs) %>%
        arrange( Id )
    
    #convert expected values to 0.01in values
    res$Expected <- round(res$Expected / 0.254) * 0.254
    
    write.csv(res, "KS_gbm_cv.csv", row.names = FALSE);zzz <- tcheck( desc='write submission file'); print(zzz)
    
}

print( get_tcheck())

