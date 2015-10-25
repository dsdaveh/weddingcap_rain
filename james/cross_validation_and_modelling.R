library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)
library(h2o)
library(fastmatch)
library(zoo)
h2o.init(nthreads=-2)

setwd("/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james")


train <- fread('../train.csv')

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}


rr_kdp <- function(kdpval) {
  rate_kdp = sign(kdpval) * 40.6 * (abs(kdpval)^0.866)
  return(rate_kdp)
}



  
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
  


make_kdp_data_dtable <- function() {
  
  #TODO:  this generates warnings such as:
  # 1: In min(Kdp, na.rm = TRUE) : no non-missing arguments to min; returning Inf
  # 2: In max(Kdp, na.rm = TRUE) : no non-missing arguments to max; returning -Inf
  
  kdp <- train[ , .(
    radardist_km = max(radardist_km, na.rm=TRUE)
    ,kdp.mean = mean(Kdp, na.rm=TRUE)
    ,kdp.median = median(Kdp, na.rm=TRUE)
    ,kdp.min = min(Kdp, na.rm=TRUE)
    ,kdp.max = max(Kdp, na.rm=TRUE)
    ,kdp.sd = sd(Kdp, na.rm=TRUE)
    ,kdp.records = .N
    ,kdp.naCounts = sum(is.na(Kdp))
    ,Measured = max(Expected)
    ,m.palmer=mpalmer(Ref, minutes_past)
  ), by=Id] %>% mutate(m.palmer.err=m.palmer-Measured,
                       rr.kdp = rr_kdp(kdp.mean),
                       rr.kdp.err = rr.kdp-Measured)
  
  save(kdp, file="../kdp.RData")
  kdp
}











#we are going to have to summarize and then desummarize data: Step 1) summarize to build predictive model, 2) desummarize to couple each entry/row with prediction
#we need to determine the "weights" between each of our variables marshall palmers equivalents and the expected variable value. e.g. use ref primarily if all values present, but if ref values are N/a then default to RefComposite 


##########FUNCTIONS###########
#Fast %in%
`%fin%` <- function(x, lkup) {
  fmatch(x, lkup, nomatch = 0L) > 0L
}

#Get the time differences between each measure
time_difference <- function(times, num_per_segment = 60) {
  n <- length(times)
  valid_time <- vector(mode="numeric", length = n)
  valid_time[1] <- times[1]
  valid_time[-1] <- diff(times, 1)
  valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
  valid_time <- valid_time / num_per_segment
  valid_time
}

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
  ((10**(dbz/10))/200) ** 0.625
}



##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

######Training Data############
tr_raw <- fread("../train.csv", select = c(
  "Id", 
  "minutes_past", 
  "radardist_km",
  "Ref", 
  "Expected")
)



tr_raw <- tr_raw[round(Expected, 4) %fin% valid_vals] #didn't work
tr_raw$dt <- time_difference(tr_raw$minutes_past)
tr_raw$mp <- marshall_palmer(tr_raw$Ref)

#Collapse to one record per Id
tr <- tr_raw[, .(
  target = log1p(mean(Expected, na.rm = T)),
  ref = mean(dt * Ref, na.rm = T),
  mp = sum(dt * mp, na.rm = T),
  rd = mean(radardist_km, na.rm = T),
  records = .N,
  naCounts = sum(is.na(Ref))
), Id]







print("training model...")
trainHex<-as.h2o(tr,destination_frame="train.hex")

feature_cols <- c("ref", "mp", "rd", "records")
gbmHex<-h2o.gbm(x=feature_cols,
                y="target",training_frame=trainHex, model_id="gbmStarter.hex",
                distribution="AUTO",
                nfolds = 0,
                seed = 23887,
                ntrees = 1130,
                max_depth = 7,
                min_rows = 10,
                learn_rate = 0.015)

rm(tr_raw)
gbmHex

print("Processing test data...")
te_raw<-fread("../input/test.csv",select=c(    
  "Id", 
  "minutes_past", 
  "radardist_km",
  "Ref", 
  "Expected")
)
te_raw$dt <- time_difference(te_raw$minutes_past)
te_raw$mp <- marshall_palmer(te_raw$Ref)

te <- te_raw[, .(
  ref = mean(dt * Ref, na.rm = T),
  mp = sum(dt * mp, na.rm = T),
  rd = mean(radardist_km),
  records = .N
),Id]

testHex<-as.h2o(te,destination_frame="test.hex")
sample_sol <-fread("../input/sample_solution.csv")
gmb_prediction <- expm1(as.data.frame(h2o.predict(gbmHex,testHex)))

res <- data.frame(
  Id = te$Id,
  Expected = 0.75 * gmb_prediction$predict + 0.25 * sample_sol$Expected
)

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254

write.csv(res, "submission.csv", row.names = FALSE, col.names = TRUE)