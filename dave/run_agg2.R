library(data.table)

source( "../team/rain_utils.R") 
tcheck.print <- T
tcheck(0)

agg_script <- '../dave/make_agg_data2.R'

load('../train_imputed_10pct.RData') 
rain_data <- train
source( agg_script)
train_agg <- rain_agg
save( train_agg, file='../train_agg2_10pct.RData')
rm( rain_data, rain_agg, train, train_agg )
tcheck(desc="created 10% agg")

load('../train_imputed.RData') 
rain_data <- train
source( agg_script)
train_agg <- rain_agg
save( train_agg, file='../train_agg2.RData')
rm( rain_data, rain_agg, train, train_agg )
tcheck(desc="created train agg")


load('../test_imputed.RData') 
rain_data <- test
source( agg_script)
test_agg <- rain_agg
save( test_agg, file='../test_agg2.RData')
rm( rain_data, rain_agg, test, test_agg )
tcheck(desc="created test agg")

print( get_tcheck())
