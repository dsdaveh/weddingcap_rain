# R utilities -  
# source("../team/rain_utils.R")

library(stringr)
library(magrittr)

if (! exists("tcheck.print")) tcheck.print = FALSE
tcheck.tx <- list( proc.time())  #deprecated
if (! exists("tcheck.df")) tcheck.df <- data.frame( stringsAsFactors = FALSE)
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
        if (tcheck.print) print( out_str)
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

extend_var_pair <- function( var_df ) {
    # expects 2 columns, 1st is var, 2nd is minutes_past ... names don't matter
    # returns a data.frame
    df <- as.data.frame (var_df)
    add_rows <- data.frame( var = c(NA,NA), mph = c(0,60))
    colnames(add_rows) <- colnames( df )
        
    if (df[ 1       , 2] !=  0)  df <- rbind(     add_rows[1, ], df)
    if (df[ nrow(df), 2] != 60)  df <- rbind( df, add_rows[2, ]    )
    return (df)
}

vimpute_var <- function( xvar, mph, allNA=xvar, method=1 ) {
    
    n_valid <- sum( ! is.na(xvar))  #count the number of non-NA's
    n <- length(xvar)
    
    #special cases
    if ( n_valid == 0 ) return(allNA)  # no non-NA's -- give up
    if ( n_valid == 1 ) return( rep( mean(xvar, na.rm=TRUE), length(xvar))) #only one value
    
    valids <- which( ! is.na( xvar ))
    d_mph <- diff( mph )
    slope <- diff( xvar[ valids ]) / diff( mph[ valids ])  # known slopes
    if ( method == 1 ) slope <- c( slope, slope[n_valid-1] )
    if ( method == 2 ) slope <- c( slope, 0 )
    start_pt <- c( valids, 61)
    
    y_t <- xvar  #output vector
    
    #extrapolate first point if necessary
    if ( is.na(xvar[1]))  {
        if (method == 1) {
            y_t[1] <- y_t[ start_pt[1] ] -  slope[1] * ( mph[ start_pt[1]] - mph[1] )
        }
        if (method == 2) {
            y_t[1] <- y_t[ start_pt[1] ]
            slope <- c( 0, slope)
            start_pt <- c( 1, start_pt)
        } 
    }
    
    #extrapolate last point if necessary
    if ( is.na(xvar[n]))  {
        last_valid <- valids[ length(valids) ]
        if (method == 1) {
            y_t[n] <- y_t[ last_valid ] +  slope[n_valid-1] * ( mph[ n] - mph[ last_valid] )
        }
        if (method == 2) {
            y_t[n] <- y_t[ last_valid ]
        } 
    }
    
    #special cases
    if ( length(xvar) == 2 ) return( y_t ) #only one value
    
    iseg <- 1
    for ( i in 2:(n-1)) {
        if (i < start_pt[iseg + 1] ) {
            y_t[i] <- slope[iseg] * d_mph[i-1] + y_t[i-1]
        } else {
            iseg <- iseg + 1
        }
    }
    
    return(y_t)
}

vimpute_agg <- function( xvar, mph, allNA=xvar, method=2, fun=identity ) {
    x2 <- extend_var_pair ( data.frame( xvar, mph ) )
    if ( length( allNA != 1 )) allNA = x2$xvar    #this is an assumption, but faster than a check using identical()
    x2$imputed <- vimpute_var( x2$xvar, x2$mph, allNA=allNA, method=method )
    agg <- sum( fun(  (x2$imputed[-1] + x2$imputed[-nrow(x2)] ) /2 ) * diff( x2$mph )/60  )
    return( agg )
}

ref_to_mm_kaggle <- function(dbz)  ((10**(dbz/10))/200) ** 0.625   #marshal_palmer
ref_to_mm_lit <- function(dbz) 0.0365*(10**(0.0625*dbz))
ref_to_mm <- ref_to_mm_kaggle
kdp_to_mm <- function(kdp)  sign(kdp) * 40.6 * (abs(kdp)^0.866)
katsumata_ref_to_mm <- function(ref) 0.027366 * ((10**(ref/10))^0.69444)
refzdr_to_mm <- function(ref, zdr) sign(zdr) * 0.00746 * ((10**(ref/10))^0.945) * (abs(zdr)^(-4.76))
kdpzdr_to_mm <- function(kdp, zdr) sign(zdr) * sign(kdp) * 136 * (abs(kdp)^0.968) * (abs(zdr)^(-2.86))

#####
# hybrid selection by handrasekar, et al (1990), Chadrasekar, et al. (1993), and extended by Ryzhkov, et al., 2005
# this func takes a single aggregated predicted rainfall for each, and uses thresholds
# to select a single prediction.
# MODIFIED from the original to use our MP calc rather than katsumata
#####
hybrid_to_mm <- function(ref_mm, refzdr_mm, kdpzdr_mm, kdp_mm) {
  #Note: There is no clear support in the literature for the specific, chosen values of _hybrid_aa, bb or cc.
  hybrid_aa = 10;
  hybrid_bb = 75;
  hybrid_cc = 100;
  if (ref_mm <= hybrid_aa) { rateHybrid = ref_mm;
  } else if (refzdr_mm <= hybrid_bb) { rateHybrid = refzdr_mm;
  } else if (refzdr_mm < hybrid_cc) {
    if (kdpzdr_mm < 0.5 * refzdr_mm) { rateHybrid = refzdr_mm;
    } else if (kdpzdr_mm > refzdr_mm) { rateHybrid = refzdr_mm;
    } else { rateHybrid = kdpzdr_mm; }
  } else {
    if (kdp_mm < 0.5 * refzdr_mm) { rateHybrid = refzdr_mm;
    } else { rateHybrid = kdp_mm; }
  }
  
  return(rateHybrid)
}

EOD <- 1
