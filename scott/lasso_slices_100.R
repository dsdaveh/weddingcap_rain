#####
# LASSO of predictions from training GBM models on slices of train in 10mm increments
# this expects that you have executed run_xgb_thresh_study.R and have a set of .CSVs
# you also need to comment out line 81: "tr <- tr[round(Expected, 4) %fin% valid_vals, ]"
# to generate CSVs above 70
# Note in the CSVs y = Expected from original training set
#####

library(glmnet)

#load first manually to get Ids in
slice_file <- 10
run_id <- sprintf("gbm_slice_%d_%d", (slice_file - 10), slice_file)
ensemble.frame <- fread(sprintf( "%s-cvtest.csv", run_id))
setnames(ensemble.frame, "Expected", sprintf( "Exp_%s", run_id))
setnames(ensemble.frame, "y", "Expected")
setcolorder(ensemble.frame, c("Id", "Expected", sprintf( "Exp_%s", run_id)))
setkey(ensemble.frame, "Id")

#load next slice files up to 100
for (slice_file in seq(20, 100, 10)) {
  run_id <- sprintf("gbm_slice_%d_%d", (slice_file - 10), slice_file)
  new.frame <- fread(sprintf( "%s-cvtest.csv", run_id))
  ensemble.frame[,sprintf( "Exp_%s", run_id) := new.frame$Expected] 
}

#fit a LASSO
x <- model.matrix(Expected~Exp_gbm_slice_0_10 + Exp_gbm_slice_10_20 + Exp_gbm_slice_20_30 +
                    Exp_gbm_slice_30_40 + Exp_gbm_slice_40_50 + Exp_gbm_slice_50_60 +
                    Exp_gbm_slice_60_70 + Exp_gbm_slice_70_80 + Exp_gbm_slice_80_90 +
                    Exp_gbm_slice_90_100, ensemble.frame)
y <- ensemble.frame$Expected
lasso.fit <- glmnet(x,y,alpha=1, family="gaussian")
plot(lasso.fit)
cv.lasso.fit <- cv.glmnet(x,y,alpha=1)
best_lambda <- cv.lasso.fit$lambda.min
best_lambda
lasso.coef <- predict(lasso.fit,type="coefficients",s=best_lambda)
ensemble.frame$LASSO_Pred <- predict(lasso.fit,s=best_lambda,type="link",newx=x)
lasso.coef
plot(cv.lasso.fit$lambda)

mae(ensemble.frame$Expected, ensemble.frame$LASSO_Pred) # 42.93526
