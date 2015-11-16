setwd('/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james')
#source ("../james/make_imputed_train_james_edits.R")
source(("../dave/make_imputed_train.R"))
source("../team/run_agg.R")
rdata_file <- "../train_agg_10pct.RData"
source("../dave/gbm_cv.R")

