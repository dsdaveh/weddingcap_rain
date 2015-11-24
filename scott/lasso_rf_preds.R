#####
# LASSO of various rainfall predictions in rain_utils
# y = Expected from original training set
#####

library(data.table)
library(glmnet)

source( "../team/rain_utils.R")

load("../train_agg-mod.RData")

# aggregate into a new table
ensemble.frame <- data.frame(Expected=train_agg$Expected, ref_mm=train_agg$Ref_rz, katsumata_ref_mm=train_agg$rr_Katsumata_ref,
                             refzdr_mm=train_agg$rr_refzdr, kdpzdr_mm=train_agg$rr_kdpzdr, kdp_mm=train_agg$Kdp_rk)
#train_agg$hybrid_mm <- hybrid_to_mm(train_agg$ref_mm, train_agg$refzdr_mm, train_agg$kdpzdr_mm, train_agg$kdp_mm)

#fit a LASSO
# TODO: not sure if this is the best approach, but model.matrix drops rows with NAs
# so this replaces NAs with -1
# but this is still not working
for (j in seq_len(ncol(ensemble.frame)))
  set(ensemble.frame,which(is.na(ensemble.frame[[j]])),j,-1)

x <- model.matrix(Expected ~ ref_mm + katsumata_ref_mm + refzdr_mm +
                    kdpzdr_mm + kdp_mm, ensemble.frame, na.action="na.pass")
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
 
mae(ensemble.frame$Expected, ensemble.frame$LASSO_Pred) # 42.93526 TODO ???
