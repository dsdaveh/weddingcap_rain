library(data.table)
library(fastmatch)
library(zoo)
library(Metrics)
library(dplyr)
library(h2o)

source( "../team/rain_utils.R")

library(devtools)
#install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")

library(h2oEnsemble)  # This will load the `h2o` R package as well

h2o.init(
    nthreads=-1,            ## -1: use all available threads
    max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

#
#
tcheck(0)
##### parameters 
run_id <- 'stack_defaults'
rdata_file <- '../train_agg2.RData'
rtest_file <- '../test_agg2.RData'

cv_frac_trn <- .7
seed <- 99 
rain_thresh <- 70

cs <- c("rd"
  , "Ref", "Ref_5x5_50th", "Ref_5x5_90th"
  , "RefComposite", "RefComposite_5x5_50th", "RefComposite_5x5_90th"
  , "Zdr", "Zdr_5x5_50th", "Zdr_5x5_90th"
  , "nrec", "naRef" 
  , "Ref_rz", "Kdp", "Kdp_rk", "rr_Katsumata_ref", "rr_refzdr", "rr_kdpzdr"
)

learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"

######Training Data############
cat("loading train from RData file ", rdata_file, "\n")
load( rdata_file )
setkey( train_agg, Id)   ; tcheck( desc='load data')

# scrub train_agg
train_NA <- train_agg[ ! is.na(Ref), median(Expected)]    #probably could just set this to 0
train_agg <- train_agg[ !is.na(Ref) , ]

cv_ix_trn <- rep( TRUE, nrow(train_agg))

cv_frac_trn <- .7
seed <- 99 
if (cv_frac_trn < 1) {
    ids <- unique(train_agg$Id)
    set.seed( seed )
    cv_ids_trn <- sample( ids, round(cv_frac_trn * length(ids)) )
    cv_ix_trn <- train_agg$Id %in% cv_ids_trn                                 ;tcheck( desc='partition cv_train')
}

trn <-     train_agg[ cv_ix_trn, ][ Expected <= rain_thresh, ]
trn.mae <- train_agg[ cv_ix_trn, ]   #for MAE checking
trn$target <- log1p( trn$Expected)
tst <- train_agg[ ! cv_ix_trn, ]

#### Load Data into H2O Cluster
df_trn <- paste0( run_id, ".trn")
df_tst <- paste0( run_id, ".tst")
df_mtr <- paste0( run_id, ".mtr")
h2o.trn <- as.h2o( trn, destination_frame = df_trn)    
h2o.mtr<-  as.h2o( trn.mae, destination_frame = df_mtr)
h2o.tst <- as.h2o( tst, destination_frame = df_tst)    ; tcheck( desc='convert to h2o df')

y_trn <- trn$Expected
y_tst <- tst$Expected
y_mtr <- trn.mae$Expected
rm(trn, tst, trn.mae, train_agg)
gc()

y <- "target"
x <- cs

#### Specify Base Learners & Metalearner
#For this example, we will use the default base learner library for `h2o.ensemble`, which includes the default H2O GLM, Random Forest, GBM and Deep Neural Net (all using default model parameter values).  We will also use the default metalearner, the H2O GLM.
#
#
#
#### Train an Ensemble
#Train the ensemble (using 5-fold internal CV) to generate the level-one data.  Note that more CV folds will take longer to train, but should increase performance.
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = h2o.trn, 
                    family = "AUTO", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))
#
#
tcheck( desc='h2o ensemble')

#### Predict on the scrubbed train set
pred <- predict(fit, h2o.trn)
mae_scrub_trn <- calc_mae( pred$pred, y_trn )
tcheck( desc= sprintf("MAE on scrubbed train = %10.6f", mae_scrub_trn))

#### Predict on the unscrubbed train set
pred <- predict(fit, h2o.mtr)
mae_cv_trn <- calc_mae( pred$pred, y_mtr )
tcheck( desc= sprintf("MAE on CV train = %10.6f", mae_cv_trn))

#### Predict 
#Generate predictions on the test set.
pred <- predict(fit, h2o.tst)
mae_cv_test <- calc_mae( pred$pred, y_tst )
tcheck( desc= sprintf("MAE on CV test = %10.6f", mae_cv_test))

calc_mae <- function (h2o_pred, y, l=1) {
    yhat <- expm1( as.data.frame(h2o_pred)[,l] )
    yhat <- ifelse( is.na(yhat), train_NA, yhat)  #fix the NA's (GLM only)
    mae( yhat, y)
}


##### Base learners test set
#We can compare the performance of the ensemble to the performance of the individual learners in the ensemble.  Again, we use the `AUC` utility function to calculate performance.
#
L <- length(learner)
mae_base <- sapply(seq(L), function(l) (MAE =calc_mae(pred$basepred, y_tst, l=l)) )
res <- data.frame(learner, mae_cv_test = mae_base ) %>% 
    rbind( data.frame( learner = paste ('H2O Ensemble', run_id, sep=':'), mae_cv_test))

out_file <- sprintf( "txt_out/%s-MAE.txt", run_id  )
write.csv(res, file=out_file, row.names = F)
print(res)
print(tcheck.df)
