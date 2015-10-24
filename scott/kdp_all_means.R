#test regression on means of all KDP columns

library(dplyr)
library(data.table)
library(ggvis)
library(h2o)

source("../team/data_prep.R")
source("../team/rain_utils.R")

#base data set with means of all KDP columns
kdp <- train[ , .(
  radardist_km = max(radardist_km, na.rm=TRUE)
  ,records = .N
  ,kdp.mean = weighted.mean(Kdp, duration, na.rm=TRUE)
  ,kdp.10thmean = weighted.mean(Kdp_5x5_10th, duration, na.rm=TRUE)
  ,kdp.50thmean = weighted.mean(Kdp_5x5_50th, duration, na.rm=TRUE)
  ,kdp.90thmean = weighted.mean(Kdp_5x5_90th, duration, na.rm=TRUE)
  ,rr.kdp = rr_kdp(Kdp, minutes_past)
  ,rr.kdp10th = rr_kdp(Kdp_5x5_10th, minutes_past)
  ,rr.kdp50th = rr_kdp(Kdp_5x5_50th, minutes_past)
  ,rr.kdp90th = rr_kdp(Kdp_5x5_90th, minutes_past)
  ,Measured = max(Expected)
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
kdp_below10 <- kdp[Measured < 10, ]
kdp_below10.sample50k <- kdp_below10[Id==Ids.sample50k,]
kdp_above70 <- kdp[ kdp$Measured > 70, ]
kdp_above70.sample50k <- kdp_above70[Id==Ids.sample50k,]


#start local H2O server
localh2o <- h2o.init(nthreads=-2)

#####
# Model on all records
#####
trainHex.all <- as.h2o(kdp, destination_frame="trainall.hex")

feature_cols <- c("radardist_km", "records", "rr.kdp", "rr.kdp10th", "rr.kdp50th", "rr.kdp90th")
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
#   MSE:  87460.22
# R2 :  0.1074587
# Mean Residual Deviance :  87460.22

gbmHex.all@model$variable_importances
# Variable Importances: 
#   variable relative_importance scaled_importance percentage
# 1 radardist_km 106132135936.000000          1.000000   0.362332
# 2      records  78724333568.000000          0.741758   0.268763
# 3   rr.kdp90th  48981487616.000000          0.461514   0.167222
# 4       rr.kdp  20895178752.000000          0.196879   0.071336
# 5   rr.kdp10th  19237330944.000000          0.181258   0.065676
# 6   rr.kdp50th  18943234048.000000          0.178487   0.064672


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
#   MSE:  990872.6
# R2 :  0.4570851
# Mean Residual Deviance :  990872.6


gbmHex.above70@model$variable_importances
# Variable Importances: 
#   variable relative_importance scaled_importance percentage
# 1 radardist_km 458443390976.000000          1.000000   0.576761
# 2      records 111638306816.000000          0.243516   0.140451
# 3   rr.kdp90th  86799482880.000000          0.189335   0.109201
# 4   rr.kdp10th  59386232832.000000          0.129539   0.074713
# 5   rr.kdp50th  42136563712.000000          0.091912   0.053011
# 6       rr.kdp  36454674432.000000          0.079518   0.045863


#####
# Plots
#####

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

