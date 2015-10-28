# R utilities -  
# source("../team/rain_utils.R")

library(stringr)
library(magrittr)

tcheck.tx <- list( proc.time())  #deprecated
tcheck.df <- data.frame( stringsAsFactors = FALSE)
tcheck.default_string <- function() sprintf( "t=%d", nrow(tcheck.df))
tcheck <- function(t=1, desc = tcheck.default_string() ) {
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
        tcheck.df <<- data.frame( elapsed = pt[3], desc = desc,stringsAsFactors = FALSE )
    } else {
        tcheck.tx <<- c( tcheck.tx, list(pt))
        tcheck.df <<- rbind( tcheck.df, data.frame( elapsed = pt[3], desc = desc, stringsAsFactors = FALSE ) )
        tn <- nrow( tcheck.df )
        elapsed_delta <- diff( tcheck.df[ c(tn-t, tn),]$elapsed )
       out_str <- ifelse ( t == 1
                            , sprintf("%f elapsed for %s", elapsed_delta
                                      , tcheck.df[tn, "desc"] )
                            , sprintf("%f elapsed from %s:%s", elapsed_delta
                                      , tcheck.df[tn, "desc"], tcheck.df[tn-t, "desc"]) )
        return( out_str )
#         tn <- length(tcheck.tx)
#         print ( tcheck.tx[[tn]] - tcheck.tx[[tn-t]]) 
    }
}
get_tcheck <- function() tcheck.df %>% mutate( delta=c( 0, diff(elapsed)) ) %>% select( desc, delta)

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

old_mpalmer <- function(ref, minutes_past) {
    
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

new_mpalmer <- function(ref, minutes_past) {
    #is there at least one valid ref value
    if ( sum( is.na(ref)) == length(ref) ) return ( -1 )

#data frame is too slow    
#     df <- data.frame( ref=ref, mp=minutes_past ) %>%
#         filter( ! is.na( ref )) %>%
#         arrange( mp )
    
    #filter
    valid <- ! is.na(ref)
    ref <- ref[valid]
    mp <- minutes_past[valid] 
    
    #arrange
    sort_min_index = order(mp)
    mp <- mp[sort_min_index]
    ref <- ref[sort_min_index]
    
    
    n <- length(ref)
    if ( mp[1] != 0  ) { 
        mp <-  c(0, mp)
        ref <- c( ref[1], ref)
        n <- n + 1
    }
    
    if ( mp[n] != 60 ) {
        mp <- c( mp, 60 )
        ref <- c( ref, ref[n])
        n <- n + 1
    }
    
    ref_int <- ( ref[-1] + ref[-n] ) /2
    hr_int <- diff( mp ) / 60
    
    mm <- sum( ((( 10^ (ref_int/10) )/200) ^ 0.625 ) * hr_int  )

#     plot( 0:60, c( 0, rep( max(ref), 60)), type="n", ylab="Ref")
#     lines( mp,  ref, type="b" )
#     data.frame( ref_int, mmperhr = ((( 10^ (ref_int/10) )/200) ^ 0.625 ), hr_int )
    
    return(mm)
}

mpalmer <- new_mpalmer #   revert with mpalmer <- old_mpalmer

transform.header <- function(str) {
    # use this for xtable with special chars in header
    str %>%
        str_replace("^X_", "_") %>%
        str_replace_all("_hash_", "#")  %>%     
        str_replace_all("_percent_", "%")  %>%     
        str_replace_all("_slash_", "/")  %>%     
        str_replace_all("_" , "&nbsp;")              # single underscore -> <space>
}

#####
#   Rain rate calculation based on Kdp, Rain rates are in mm/hr. 
#   sourced from https://www.eol.ucar.edu/projects/dynamo/spol/parameters/rain_rate/rain_rates.html
#     RATE_KDP = sign(KDP) * kdp_aa * (|KDP| ** kdp_bb).
#   where kdp_aa = 40.6 and kdp_bb = 0.866
#####
rr_kdp <- function(kdpval, minutes_past) {
  
  # order reflectivity values and minutes_past
  sort_min_index = order(minutes_past)
  minutes_past <- minutes_past[sort_min_index]
  kdpval <- kdpval[sort_min_index]
  
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
  
  # calculate hourly rain rates using kdp formula weighted by valid times
  sum <- 0
  for (i in seq(length(kdpval))) {
    if (!is.na(kdpval[i])) {
      mmperhr <- sign(kdpval[i]) * 40.6 * (abs(kdpval[i])^0.866)
      sum <- sum + mmperhr * valid_time[i]
    }
  }
  
  return(sum)
  
}

