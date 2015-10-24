# R utilities -  
# source("../team/rain_utils.R")

library(stringr)
library(magrittr)

tcheck.tx <- list( proc.time())
tcheck <- function(t=1) {
    # t=0 to reset counter, t=1 incremental time output,  t=n time difference from n intervals
    #
    # use:
    # tcheck(0) #reset the counter
    # <computation 1>
    # tcheck()
    # <computation 2>
    # tcheck()
    # tcheck(2)  # for total time
    #
    t <- min( t, length(tcheck.tx))
    pt <- proc.time()
    if (t == 0) { 
        tcheck.tx <<- list( proc.time()) 
    } else {
        tcheck.tx <<- c( tcheck.tx, list(pt))
        tn <- length(tcheck.tx)
        print ( tcheck.tx[[tn]] - tcheck.tx[[tn-t]]) 
    }
}

durationscaled <- function(duration) {
  thefactor <- 1.0 / sum(duration)
  scaled <- duration*thefactor
  return(scaled)
}

duration <- function(minutes_past) {
  
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
  
  return(valid_time)
  
}

mpalmer <- function(ref, minutes_past) {
    
    #is there at least one valid ref value
    if ( sum( is.na(ref)) == length(ref) ) return ( -1 )
    
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


transform.header <- function(str) {
    # use this for xtable with special chars in header
    str %>%
        str_replace("^X_", "_") %>%
        str_replace_all("_hash_", "#")  %>%     
        str_replace_all("_percent_", "%")  %>%     
        str_replace_all("_slash_", "/")  %>%     
        str_replace_all("_" , "&nbsp;")              # single underscore -> <space>
}