library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)
library(xgboost)
library(ltm)
source("../team/rain_utils.R")
source("../team/data-prep.R")

rdata_file <- "../train.Rdata" ### set this to a file name containing raw training data
# train <- fread("../train.csv")
# train <- data.table[train]
# save(train, file="../train.Rdata")
#run replacing new_mpalmer with generic method=2
gen_mpalmer <- function( ref, mph ) vimpute_agg( ref, mph, method=2, fun=ref_to_mm)
chg_mpalmer <- gen_mpalmer
set_seed <- 1999
source("../dave/KS_gbm_cv.R")   # save the output into a text file and write the name of the file here for your records
#run replacing new_mpalmer with generic method=1
gen_mpalmer <- function( ref, mph ) vimpute_agg( ref, mph, method=1, fun=ref_to_mm)
chg_mpalmer <- gen_mpalmer
source("../dave/KS_gbm_cv.R")   # save the output into a text file and write the name of the file here for your records
#run with default new_mpalmer, which should be very close (but isn't identical) to method=2
rm(chg_mpalmer)
source("../dave/KS_gbm_cv.R")   # save the output into a text file and write the name of the file here for your records
