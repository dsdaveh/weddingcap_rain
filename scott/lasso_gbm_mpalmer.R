#####
# LASSO of gbm_cv and mpalmer
# y = Expected from original training set
#####

library(data.table)
library(glmnet)

source( "../team/rain_utils.R")

load("../train_agg.RData")
rdata_file <- "../train_agg.RData"

# get gbm predictions
create_submission <- TRUE
run_id <- "gbm_defaults"
source ('../dave/gbm_cv.R')

# load predictions
ensemble.frame <- fread("gbm_defaults-cvtest.csv")
setnames(ensemble.frame, "Expected", "pred_gbm")
setnames(ensemble.frame, "y", "Expected")
setcolorder(ensemble.frame, c("Id", "Expected", "pred_gbm"))

# add mpalmer predictions
ensemble.frame <- inner_join(ensemble.frame, select(train_agg, Id, Ref), by="Id")
ensemble.frame$pred_mpalmer <- ref_to_mm(ensemble.frame$Ref)
ensemble.frame <- ensemble.frame[,Ref:=NULL]

#fit a LASSO
x <- model.matrix(Expected ~ pred_gbm + pred_mpalmer, ensemble.frame)
y <- ensemble.frame$Expected
lasso.fit <- glmnet(x,y,alpha=1, family="gaussian")
plot(lasso.fit)
cv.lasso.fit <- cv.glmnet(x,y,alpha=1)
best_lambda <- cv.lasso.fit$lambda.min
best_lambda
lasso.coef <- predict(lasso.fit,type="coefficients",s=best_lambda)
ensemble.frame$LASSO_Pred <- predict(lasso.fit,s=best_lambda,type="class",newx=x)
lasso.coef
plot(cv.lasso.fit$lambda)

mae(ensemble.frame$Expected, ensemble.frame$LASSO_Pred) # 42.6799 on full; 36.82725 on 10%
