#####
# LASSO of gbm_cv , h2o_rev_cv, and rain_calcs
# y = Expected from original training set
#####

library(data.table)
library(dplyr)
library(glmnet)

source( "../team/rain_utils.R")

# load predictions
# TODO: add generation info
xgbm <- fread("csv_out/xgbm_f70_seed99_99-cvtest.csv") %>% rename( xgbm = Expected)
h2orf <- fread("csv_out/rf_brazil_f70_1-cvtest.csv") %>% rename( h2orf = Expected) %>% select( Id, h2orf)

calc_file <- "../rainfall_calculations.RData"
load(file=calc_file)
rain_calcs <- rain_calcs %>% select(Id, Ref_rz, Kdp_rk, rr_Katsumata_ref)

setkey(kaggle_mpalmer, Id)
setkey(xgbm, Id)
setkey(h2orf, Id)
setkey(rain_calcs, Id)

ensemble.frame <- xgbm %>%
    left_join( h2orf, by="Id") %>%
    left_join( rain_calcs, by="Id") %>%
    mutate(target = log1p(y))
    
#fit a LASSO
x <- model.matrix(y ~ xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref , ensemble.frame)
y <- ensemble.frame$y
lasso.fit <- glmnet(x,y,alpha=1, family="gaussian")
plot(lasso.fit)
cv.lasso.fit <- cv.glmnet(x,y,alpha=1)
best_lambda <- cv.lasso.fit$lambda.min
best_lambda
lasso.coef <- predict(lasso.fit,type="coefficients",s=best_lambda)
ensemble.frame$LASSO_Pred <- predict(lasso.fit,s=best_lambda,type="class",newx=x)
ensemble.frame$final <- round( ensemble.frame$LASSO_Pred / .0254 ) * .0254
lasso.coef
plot(cv.lasso.fit$lambda)

mae(ensemble.frame$y, ensemble.frame$final) # 41.90745
mae(ensemble.frame$y, ensemble.frame$xgbm) # 23.36106
mae(ensemble.frame$y, ensemble.frame$h2orf) # 23.16974
mae(ensemble.frame$y, ensemble.frame$Ref_rz) # 23.66663
mae(ensemble.frame$y, ensemble.frame$Kdp_rk) # 58.63498
mae(ensemble.frame$y, ensemble.frame$rr_Katsumata_ref) # 24.05466

#compare to straight average
ensemble.frame$avg <- with(ensemble.frame, (xgbm + h2orf + Ref_rz + Kdp_rk + rr_Katsumata_ref)/ 5 )
mae(ensemble.frame$y, ensemble.frame$avg) # 29.63952
