rdata_file <- "train_10pct.RData"
source("KS_gbm_cv.R")  #KS_gbm_cv_OUT_f07_10pct_mpnew.txt

#run replacing new_mpalmer with generic
gen_mpalmer <- function( ref, mph ) vimpute_agg( ref, mph, method=2, fun=ref_to_mm)
chg_mpalmer <- gen_mpalmer
source("KS_gbm_cv.R")   #KS_gbm_cv_OUT_f07_10pct_mpgen.txt

#run replacing new_mpalmer with generic and new MP formula
gen_mpalmer <- function( ref, mph ) vimpute_agg( ref, mph, method=2, fun=ref_to_mm_lit)
chg_mpalmer <- gen_mpalmer
source("KS_gbm_cv.R")   #

#run after adding kdp
rm(chg_mpalmer)
source("KS_gbm_cv.R")   #KS_gbm_cv_OUT_f07_10pct_mpkdp.txt

rdata_file <- "train_full.RData"
source("KS_gbm_cv.R")   #KS_gbm_cv_OUT_f07_full_mpkdp.txt

cv_frac_trn <- 1
create_submission <- TRUE
source("KS_gbm_cv.R")   #KS_gbm_cv_OUT_f10_full_mpkdp.txt

rm( cv_frac_trn, create_submission)
rdata_file <- "train_10pct.RData"
source("KS_gbm_cv2.R")  #KS_gbm_cv_OUT_f07_10pct_cv2a.txt

rdata_file <- "train_full.RData"
source("KS_gbm_cv2.R")   #.txt

cv_frac_trn <- 1
create_submission <- TRUE
source("KS_gbm_cv2.R")   #.txt


