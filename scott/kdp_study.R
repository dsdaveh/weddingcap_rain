#####
#   Study of Kdp, basic data set plus rain rate calculation 
#####

library(dplyr)
library(data.table)
library(ggvis)

source("../team/rain_utils.R")

#run once
train <- fread('../train.csv')
results <- train %>% group_by(Id) %>% summarize(m.palmer=mpalmer(Ref, minutes_past))

#####
#   Rain rate calculation based on Kdp, Rain rates are in mm/hr. 
#   sourced from https://www.eol.ucar.edu/projects/dynamo/spol/parameters/rain_rate/rain_rates.html
#     RATE_KDP = sign(KDP) * kdp_aa * (|KDP| ** kdp_bb).
#   where kdp_aa = 40.6 and kdp_bb = 0.866
#####
rr_kdp <- function(kdpval) {
  rate_kdp = sin(kdpval) * 40.6 * (abs(kdpval)^0.866)
  return(rate_kdp)
}


make_kdp_data_dframe <- function() {
  
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
  kdp.frame
}

make_kdp_data_dtable <- function() {
  
  #TODO:  this generates warnings such as:
  # 1: In min(Kdp, na.rm = TRUE) : no non-missing arguments to min; returning Inf
  # 2: In max(Kdp, na.rm = TRUE) : no non-missing arguments to max; returning -Inf
  
  kdp <- train[ , .(
    kdp.mean = mean(Kdp, na.rm=TRUE)
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

if (!exists("kdp")) {
  if (file.exists("kdp.RData")) {
    cat("loading kdp from RData file\n")
    load("../kdp.RData")
  } else {
    cat("loading kdp from CSV\n")
    kdp <- make_kdp_data_dtable()
  }
}

#create random sample of 1000 Ids (and then all associated rows) for testing ops
if (!exists("kdp.sample1000")) {
  set.seed(498)
  Ids.sample1000 <- sample(unique(kdp$Id), 1000)
  kdp.sample1000 <- kdp[Id==Ids.sample1000,]
}

kdp_valid <- kdp[ ! is.nan(kdp.mean) & (Measured < 70), ]

range(kdp_valid$kdp.mean) # -70.48 122.44
hist(kdp_valid$kdp.mean)
sd(kdp_valid$kdp.mean) # 2.749107
mean(kdp_valid$kdp.mean) # 0.05035855
median(kdp_valid$kdp.mean) # 0.02916335

range(kdp_valid$rr.kdp) # -2221.086  1975.011
hist(kdp_valid$rr.kdp)



