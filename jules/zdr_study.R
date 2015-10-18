## Use with caution....not fully QA'd yet.

library(dplyr)
library(data.table)



mpalmer <- function(ref, minutes_past) {
  
  # order reflectivity values and minutes_past
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  ref <- ref[sort_min_index]
  
  # calculate the length of time for which each reflectivity value is valid
  valid_time <- rep(0, length(minutes_past))
  valid_time[1] <- minutes_past[1]
  if (length(valid_time) > 1) {
    for (i in seq(2, length(minutes_past))) {
      valid_time[i] <- minutes_past[i] - minutes_past[i-1]
    }
    valid_time[length(valid_time)] = valid_time[length(valid_time)] + 60 - sum(valid_time)
  } else {
    # if only 1 observation, make it valid for the entire hour
    valid_time <- 60
  }
  
  valid_time = valid_time / 60
  
  # calculate hourly rain rates using marshall-palmer weighted by valid times
  sum <- 0
  for (i in seq(length(ref))) {
    if (!is.na(ref[i])) {
      mmperhr <- ((10^(ref[i]/10))/200) ^ 0.625
      sum <- sum + mmperhr * valid_time[i]
    }
  }
  
  return(sum)
  
}

train <- fread('../train.csv')
results <- train %>% group_by(Id) %>% summarize(yhat=mpalmer(Ref, minutes_past))


trainzdr <- train %>% select(Id, Zdr, Expected)

zdr <- trainzdr %>% group_by(Id) %>% summarize(
  mean=mean(Zdr, na.rm=TRUE), 
  median=median(Zdr, na.rm=TRUE),
  min=min(Zdr, na.rm=TRUE),
  max=max(Zdr, na.rm=TRUE),
  Std=sd(Zdr, na.rm=TRUE),
  Measured=max(Expected)
) %>% left_join(results, by="Id") %>% mutate(err=yhat-Measured)

save(zdr, file="zdr.RData")

zdrclean <- zdr[!is.nan(mean) & (Measured<70),]

range(zdrclean$mean)
hist(zdrclean$mean)
sd(zdrclean$mean)
mean(zdrclean$mean)

with(zdrclean, plot(mean,Measured, type="p"))


# Table a random sample of records first!
#plot(Rho$err, Rho$mean)
