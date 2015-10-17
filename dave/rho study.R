## Use with caution....not fully QA'd yet.

library(dplyr)
library(data.table)

if (!exists("Rho")) {
  if (file.exists("../Rho.Rdata")) {
    cat("loading train from Rdata file\n")
    load("../Rho.Rdata")
  } else {
    cat("loading train from CSV\n")
    train <- fread('../train.csv')
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
    
    results <- train %>% group_by(Id) %>% summarize(yhat=mpalmer(Ref, minutes_past))
    
    #write.csv(results, file='RhoHV.csv', row.names=FALSE)
    
    trainRhoHV <- train %>% select(Id, RhoHV, Expected)
    
    Rho <- trainRhoHV %>% group_by(Id) %>% summarize(
      mean=mean(RhoHV, na.rm=TRUE), 
      median=median(RhoHV, na.rm=TRUE),
      min=min(RhoHV, na.rm=TRUE),
      max=max(RhoHV, na.rm=TRUE),
      Std=sd(RhoHV, na.rm=TRUE),
      Measured=max(Expected)
    ) %>% left_join(results, by="Id") %>% mutate(err=yhat-Measured)
    
    save(Rho, file="Rho.RData")

  }
}

# Table a random sample of records first!
#plot(Rho$err, Rho$mean)
