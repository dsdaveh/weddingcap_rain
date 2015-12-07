library (ggvis)
library (tidyr)


rdata_file <- '../train_agg3.RData'
# rdata_file <- '../train_agg3.RData'
rdata10_file <- '../train_agg3_10pct.RData'
rtest_file <- '../test_agg3.RData'


run_id <- 'GSE_h2orf_10pct'
set_ntrees <- 500
h2o_script <- '../dave/GSE_h2orf_cvtrain.R'
create_submission <- TRUE
cv_frac_trn <- 1
tcheck.print <- TRUE
set_rain_thresh <- 69

run_time <- numeric() 

load(file = rdata10_file)
set_cs  <- names(train_agg)[-c(1, ncol(train_agg))]
set_seed <- 99

source (h2o_script)
print( get_tcheck())
print( sum( get_tcheck()$delta ) )

