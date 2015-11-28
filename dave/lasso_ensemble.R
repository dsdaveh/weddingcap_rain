#####
# LASSO of gbm_cv , h2o_rev_cv, and rain_calcs
# y = Expected from original training set
#####

library(data.table)
library(dplyr)
library(glmnet)

source( "../team/rain_utils.R")

###### parameters 
def_rain_thresh <- 65
rain_thresh <- ifelse( exists("set_rain_thresh"), set_rain_thresh, def_rain_thresh)

def_submit <- FALSE
create_submission <- ifelse( exists("set_submit"), set_submit, def_submit)
###############

# load predictions
# TODO: add generation info
xgbm_file <- sprintf("csv_out/xgbm_f70_seed%d-cvtest.csv", run_seed)
h2orf_file <- sprintf("csv_out/rf_brazil_f70_1_seed%d-cvtest.csv", run_seed)
# print(xgbm_file)
# print(h2orf_file)
xgbm <- fread( xgbm_file ) %>% rename( xgbm = Expected)
h2orf <- fread(h2orf_file) %>% rename( h2orf = Expected) %>% select( Id, h2orf)

calc_file <- "../rainfall_calculations.RData"
load(file=calc_file)
rain_calcs <- rain_calcs %>% select(Id, Ref_rz, Kdp_rk, rr_Katsumata_ref)

setkey(xgbm, Id)
setkey(h2orf, Id)
setkey(rain_calcs, Id)

ensemble.frame <- xgbm %>%
    left_join( h2orf, by="Id") %>%
    left_join( rain_calcs, by="Id") %>%
    mutate(target = log1p(y))    # not used (yet)
ensemble.clean <- ensemble.frame[ y <= rain_thresh]

#fit a LASSO
x <- model.matrix(y ~ xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref , ensemble.clean)
y <- ensemble.clean$y
cv.lasso.fit <- cv.glmnet(x,y,alpha=1, type.measure = "mae")
#plot(cv.lasso.fit)
best_lambda <- cv.lasso.fit$lambda.min
best_lambda
lasso.coef <- predict(cv.lasso.fit,type="coefficients",s=best_lambda)
lasso.coef
#plot(cv.lasso.fit$lambda)
ensemble.clean$LASSO_Pred <- predict(cv.lasso.fit,s=best_lambda,type="response",newx=x)
ensemble.clean$final <- round( ensemble.clean$LASSO_Pred / .0254 ) * .0254

mae(ensemble.clean$y, ensemble.clean$LASSO_Pred) # 2.335976
mae(ensemble.clean$y, ensemble.clean$final) # 2.336021

x <- model.matrix(y ~ xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref , ensemble.frame)
ensemble.frame$LASSO_Pred <- predict(cv.lasso.fit,s=best_lambda,type="response",newx=x)
ensemble.frame$final <- round( ensemble.frame$LASSO_Pred / .0254 ) * .0254

mae(ensemble.frame$y, ensemble.frame$LASSO_Pred) # 23.36415
mae(ensemble.frame$y, ensemble.frame$final) # 23.36419

mae(ensemble.frame$y, ensemble.frame$xgbm) # 23.36106
mae(ensemble.frame$y, ensemble.frame$h2orf) # 23.16974
mae(ensemble.frame$y, ensemble.frame$Ref_rz) # 23.66663
mae(ensemble.frame$y, ensemble.frame$Kdp_rk) # 58.63498
mae(ensemble.frame$y, ensemble.frame$rr_Katsumata_ref) # 24.05466

#compare to straight average
ensemble.frame$avg <- with(ensemble.frame, (xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref)/ 5 )
mae(ensemble.frame$y, ensemble.frame$avg) # 29.63952

if( create_submission ) {
    xgbm_file <- gsub("-cvtest", "", xgbm_file)
    h2orf_file <- gsub("-cvtest", "", h2orf_file)
    xgbm <- fread( xgbm_file ) %>% rename( xgbm = Expected)
    h2orf <- fread(h2orf_file) %>% rename( h2orf = Expected) %>% select( Id, h2orf)
    
    calc_file <- "../rainfall_calculations-test.RData"
    load(file=calc_file)
    rain_calcs <- rain_calcs %>% select(Id, Ref_rz, Kdp_rk, rr_Katsumata_ref)
    
    setkey(xgbm, Id)
    setkey(h2orf, Id)
    setkey(rain_calcs, Id)
    
    ensemble.test <- xgbm %>%
        left_join( h2orf, by="Id") %>%
        left_join( rain_calcs, by="Id")
    y <- rep(0, nrow(ensemble.test))
    x <- model.matrix(y ~ xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref , ensemble.test)
    ensemble.test$LASSO_Pred <- predict(cv.lasso.fit,s=best_lambda,type="response",newx=x)
    ensemble.test$final <- round( ensemble.test$LASSO_Pred / .0254 ) * .0254
    ensemble.test <- ensemble.test %>% select( Id, Expected = final )
    write.csv(ensemble.test
          , file=sprintf("csv_out/lasso_ensemble_seed%d_rt%d.csv", run_seed, rain_thresh)
          , row.names=F )

}

