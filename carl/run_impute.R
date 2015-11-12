# Create a baseline run with MPalmer and xgmb


#
source ("../dave/make_imputed_train.R")

# creates 3 aggregated datasets off the umputed data sets
source("../team/run_agg.R")
rdata_file <- "../train_agg_10pct.RData"
source("../dave/gbm_cv.R")