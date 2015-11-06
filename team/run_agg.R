source( "../team/rain_utils.R") 
tcheck(0)

load('../train_imputed_10pct.RData') 
rain_data <- train
source('../dave/make_agg_data.R')
train_agg <- rain_agg
save( train_agg, file='../train_agg_10pct.RData')
rm( rain_data, rain_agg, train, train_agg )
tcheck(desc="created 10% agg")

load('../train_imputed.RData') 
rain_data <- train
source('../dave/make_agg_data.R')
train_agg <- rain_agg
save( train_agg, file='../train_agg.RData')
rm( rain_data, rain_agg, train, train_agg )
tcheck(desc="created train agg")


load('../test_imputed.RData') 
rain_data <- test
source('../dave/make_agg_data.R')
test_agg <- rain_agg
save( test_agg, file='../test_agg.RData')
rm( rain_data, rain_agg, test, test_agg )
tcheck(desc="created test agg")
