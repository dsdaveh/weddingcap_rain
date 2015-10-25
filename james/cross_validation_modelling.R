library(dplyr)
library(data.table)
library(ggvis)

setwd("/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james")

source("../team/rain_utils.R")

#run once
train <- fread('../train.csv')
#results <- train %>% group_by(Id) %>% summarize(m.palmer=mpalmer(Ref, minutes_past))




#####
#   Rain rate calculation based on Kdp, Rain rates are in mm/hr. 
#   sourced from https://www.eol.ucar.edu/projects/dynamo/spol/parameters/rain_rate/rain_rates.html
#     RATE_KDP = sign(KDP) * kdp_aa * (|KDP| ** kdp_bb).
#   where kdp_aa = 40.6 and kdp_bb = 0.866
#####
rr_kdp <- function(kdpval) {
  rate_kdp = sign(kdpval) * 40.6 * (abs(kdpval)^0.866)
  return(rate_kdp)
}


#make_kdp_data_dframe <- function() {

#TODO:  this generates warnings such as:
# 1: In min(Kdp, na.rm = TRUE) : no non-missing arguments to min; returning Inf
# 2: In max(Kdp, na.rm = TRUE) : no non-missing arguments to max; returning -Inf

kdp.frame <- train %>% group_by(Id) %>% summarize(
  kdp.mean = mean(Kdp, na.rm=TRUE)
  ,kdp.median = median(Kdp, na.rm=TRUE)
  ,kdp.min = min(Kdp, na.rm=TRUE)
  ,kdp.max = max(Kdp, na.rm=TRUE)
  ,kdp.sd = sd(Kdp, na.rm=TRUE)
  ,kdp.records = .N
  ,kdp.naCounts = sum(is.na(Kdp))
  ,Measured = max(Expected)
  ,m.palmer=mpalmer(Ref, minutes_past)
) %>% mutate(m.palmer.err=m.palmer-Measured,
             rr.kdp = rr_kdp(kdp.mean),
             rr.kdp.err = rr.kdp-Measured)

#save(kdp, file="../kdp.RData")
# kdp.frame
#}


#breaking into validation
set.seed(5)
random_sample <- sample(1180945, 826662)

#simple linear model - 1 variable m.palmer variable
lm.fit <- lm(Measured ~ m.palmer, data = kdp.frame, subset = random_sample)
MAE <- mean(abs(kdp.frame$Measured - predict(lm.fit, kdp.frame))[-random_sample])
#MAE
print(MAE)


#decision tree
library(tree)
set.seed(5)
tree.kdp.frame <- tree(Measured~., kdp.frame, subset = random_sample)
summary(tree.kdp.frame)
plot(tree.kdp.frame)
text(tree.kdp.frame, pretty=0)

yhat <- predict(tree.kdp.frame, newdata = kdp.frame[-random_sample,])
kdp.frame.validation <- kdp.frame$Expected[-random_sample,]
plot(yhat, kdp.frame.validation)
abline(0,1)
mean(abs(yhat-kdp.frame.validation))
print(mean)

#random forest - contains errors
library(randomForest)
set.seed(5)
forest.kdp.frame = randomForest(Measured~m.palmer.err + rr.kdp.err,data= kdp.frame, subset = random_sample, 
                                mtry =13, importance = TRUE)
forest.kdp.frame
(bag.boston=randomForest(medv∼.,data=Boston,subset=train,
                         mtry=13,importance =TRUE))
yhat.forest <- predict(forest.kdp.frame, newdata = kdp.frame[-random_sample,])
plot(yhat.forest, kdp.frame.validation)
abline(0,1)
mean((yhat.forest - kdp.frame.validation)^2)


