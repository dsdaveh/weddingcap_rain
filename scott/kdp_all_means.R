######
#
#  test model using rain rate predictions based on means of each KDP column,
#  and an ensemble together with Marshall-Palmer
#
######

library(dplyr)
library(data.table)
library(ggvis)
library(h2o)

source("../team/data_prep.R")
source("../team/rain_utils.R")

#base data set with duration-weighted means of all KDP columns
kdp <- train[ , .(
  radardist_km = max(radardist_km, na.rm=TRUE)
  ,records = .N
  ,ref.mean = weighted.mean(Ref, duration, na.rm=TRUE)
  ,kdp.mean = weighted.mean(Kdp, duration, na.rm=TRUE)
  ,kdp.10thmean = weighted.mean(Kdp_5x5_10th, duration, na.rm=TRUE)
  ,kdp.50thmean = weighted.mean(Kdp_5x5_50th, duration, na.rm=TRUE)
  ,kdp.90thmean = weighted.mean(Kdp_5x5_90th, duration, na.rm=TRUE)
  ,rr.kdp = rr_kdp(Kdp, minutes_past)
  ,rr.kdp10th = rr_kdp(Kdp_5x5_10th, minutes_past)
  ,rr.kdp50th = rr_kdp(Kdp_5x5_50th, minutes_past)
  ,rr.kdp90th = rr_kdp(Kdp_5x5_90th, minutes_past)
  ,Measured = max(Expected)
  ,m.palmer=mpalmer(Ref, minutes_past)
), by=Id] %>% mutate(rr.kdp.err = rr.kdp-Measured,
                     rr.kdp10th.err = rr.kdp10th-Measured,
                     rr.kdp50th.err = rr.kdp50th-Measured,
                     rr.kdp90th.err = rr.kdp90th-Measured)

save(kdp, file="../kdp.RData")

# subsets
#create random sample of 50,000 valid Ids (and then all associated rows) for testing plots
if (!exists("kdp.sample50k")) {
  set.seed(498)
  Ids.sample50k <- sample(unique(kdp$Id), 50000)
  kdp.sample50k <- kdp[Id==Ids.sample50k,]
}
# these are not perfect, but good enough
kdp_below10 <- kdp[Measured < 10, ]
kdp_below10.sample50k <- kdp_below10[Id==Ids.sample50k,]
kdp_above70 <- kdp[Measured > 70, ]
kdp_above70.sample50k <- kdp_above70[Id==Ids.sample50k,]


#start local H2O server
localh2o <- h2o.init(nthreads=-2)

#####
# Model on all records
#####
trainHex.all <- as.h2o(kdp, destination_frame="trainall.hex")

feature_cols <- c("radardist_km", "records", "ref.mean", "rr.kdp", "rr.kdp10th", "rr.kdp50th", "rr.kdp90th")
gbmHex.all<-h2o.gbm(x=feature_cols,
                y="Measured",training_frame=trainHex.all, model_id="gbmall.hex",
                distribution="AUTO",
                nfolds = 0,
                seed = 23887,
                ntrees = 1130,
                max_depth = 7,
                min_rows = 10,
                learn_rate = 0.015)

gbmHex.all
# ** Reported on training data. **
#   
#   MSE:  85054.03
# R2 :  0.1320141
# Mean Residual Deviance :  85054.03

gbmHex.all@model$variable_importances
# Variable Importances: 
#   variable relative_importance scaled_importance percentage
# 1 radardist_km 124944228352.000000          1.000000   0.347199
# 2     ref.mean  95150440448.000000          0.761543   0.264407
# 3      records  45097107456.000000          0.360938   0.125317
# 4   rr.kdp90th  43148029952.000000          0.345338   0.119901
# 5   rr.kdp50th  17415364608.000000          0.139385   0.048394
# 6       rr.kdp  17322319872.000000          0.138640   0.048136
# 7   rr.kdp10th  16785803264.000000          0.134346   0.046645

# predict the train data
prediction.all = h2o.predict(gbmHex.all, newdata=trainHex.all)
 
# Copy predictions from H2O to R
pred.all = as.data.frame(prediction.all)

# not as good as MP overall
mae(kdp$Measured, pred.all$predict) # 61.57368

#####
# Model just above 70mm
#####
trainHex.above70 <- as.h2o(kdp_above70, destination_frame="trainabove70.hex")
gbmHex.above70<-h2o.gbm(x=feature_cols,
                y="Measured",training_frame=trainHex.above70, model_id="gbmabove70.hex",
                distribution="AUTO",
                nfolds = 0,
                seed = 23887,
                ntrees = 1130,
                max_depth = 7,
                min_rows = 10,
                learn_rate = 0.015)

gbmHex.above70
# ** Reported on training data. **
#   
# MSE:  907050.3
# R2 :  0.5030127
# Mean Residual Deviance :  907050.3


gbmHex.above70@model$variable_importances
# Variable Importances: 
#   variable relative_importance scaled_importance percentage
# 1 radardist_km 487141212160.000000          1.000000   0.556896
# 2     ref.mean 127975874560.000000          0.262708   0.146301
# 3      records  86850666496.000000          0.178286   0.099287
# 4   rr.kdp90th  60856913920.000000          0.124927   0.069571
# 5   rr.kdp10th  45601382400.000000          0.093610   0.052131
# 6   rr.kdp50th  34133200896.000000          0.070068   0.039021
# 7       rr.kdp  32184569856.000000          0.066068   0.036793

# preduct just above 70 
prediction.above70 = h2o.predict(gbmHex.above70, newdata=trainHex.above70)

# Copy predictions from H2O to R
pred.above70 = as.data.frame(prediction.above70)

# way better than MP above 70
mae(kdp_above70$Measured, pred.above70$predict) # 579.4121

# use the above 70 model to predict all
prediction.alluseabove70 = h2o.predict(gbmHex.above70, newdata=trainHex.all)

# Copy predictions from H2O to R
pred.alluseabove70 = as.data.frame(prediction.alluseabove70)

# worse than using MP on everything
mae(kdp$Measured, pred.alluseabove70$predict) # 671.6192

#####
# Ensembles
#####

#build various ensembles that combine mpalmer and Kdp to generate a prediction
ensembleall.frame <- data.frame(kdp$Measured, kdp$ref.mean, kdp$m.palmer, pred.all$predict, pred.alluseabove70$predict)
#ens1 - use Ref=20 as a cutoff
ensembleall.frame$ens1 <- ifelse(is.nan(ensembleall.frame$kdp.ref.mean), ensembleall.frame$kdp.m.palmer,
                              ifelse(ensembleall.frame$kdp.ref.mean < 20, ensembleall.frame$kdp.m.palmer,
                                ensembleall.frame$pred.alluseabove70.predict))
#ens2 - use a ratio
ensembleall.frame$ens2 <- ifelse((ensembleall.frame$pred.alluseabove70.predict/ensembleall.frame$pred.all.predict)<20, ensembleall.frame$pred.alluseabove70.predict,
                                 ensembleall.frame$kdp.m.palmer)

#####
# generate test predictions to submit
#####

test.mod <- test[ , .(
  radardist_km = max(radardist_km, na.rm=TRUE)
  ,records = .N
  ,ref.mean = weighted.mean(Ref, duration, na.rm=TRUE)
  ,kdp.mean = weighted.mean(Kdp, duration, na.rm=TRUE)
  ,kdp.10thmean = weighted.mean(Kdp_5x5_10th, duration, na.rm=TRUE)
  ,kdp.50thmean = weighted.mean(Kdp_5x5_50th, duration, na.rm=TRUE)
  ,kdp.90thmean = weighted.mean(Kdp_5x5_90th, duration, na.rm=TRUE)
  ,rr.kdp = rr_kdp(Kdp, minutes_past)
  ,rr.kdp10th = rr_kdp(Kdp_5x5_10th, minutes_past)
  ,rr.kdp50th = rr_kdp(Kdp_5x5_50th, minutes_past)
  ,rr.kdp90th = rr_kdp(Kdp_5x5_90th, minutes_past)
  ,m.palmer=mpalmer(Ref, minutes_past)
), by=Id]

test.all <- as.h2o(test.mod, destination_frame="testall.hex")

# predict the test set
prediction.testall = h2o.predict(gbmHex.all, newdata=test.all)

# Copy predictions from H2O to R
pred.testall = as.data.frame(prediction.testall)

#build the ensemble
ensembletestall.frame <- data.frame(test.mod$ref.mean, test.mod$m.palmer, pred.testall$predict)
ensembletestall.frame$ens1 <- ifelse(is.nan(ensembletestall.frame$test.mod.ref.mean), ensembletestall.frame$test.mod.m.palmer,
                                 ifelse(ensembletestall.frame$test.mod.ref.mean < 15, ensembletestall.frame$test.mod.m.palmer,
                                        ensembletestall.frame$pred.testall.predict))


results <- data.frame(
  Id = test.mod$Id,
  Expected = ensembletestall.frame$ens1
)

write.csv(results, "../submission.csv", row.names = FALSE, col.names = TRUE)

#####
# Plots
#####

df %>% ggvis(~x, ~y) %>% layer_paths()


#Plot values vs. Expected overall
kdp.sample50k %>% ggvis( ~rr.kdp, ~Measured) %>% layer_points()
kdp.sample50k %>% ggvis( ~rr.kdp10th, ~Measured) %>% layer_points() # interesting
kdp.sample50k %>% ggvis( ~rr.kdp50th, ~Measured) %>% layer_points() # interesting
kdp.sample50k %>% ggvis( ~rr.kdp90th, ~Measured) %>% layer_points()

#look below 10
kdp_below10 %>% ggvis( ~rr.kdp, ~Measured) %>% layer_points()
kdp_below10 %>% ggvis( ~rr.kdp10th, ~Measured) %>% layer_points()
kdp_below10 %>% ggvis( ~rr.kdp50th, ~Measured) %>% layer_points()
kdp_below10 %>% ggvis( ~rr.kdp90th, ~Measured) %>% layer_points()

#look above 70
kdp_above70 %>% ggvis( ~rr.kdp, ~Measured) %>% layer_points()
kdp_above70 %>% ggvis( ~rr.kdp10th, ~Measured) %>% layer_points()
kdp_above70 %>% ggvis( ~rr.kdp50th, ~Measured) %>% layer_points()
kdp_above70 %>% ggvis( ~rr.kdp90th, ~Measured) %>% layer_points()

