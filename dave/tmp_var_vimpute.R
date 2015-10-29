library(data.table)

vimpute_var <- function( xvar, mph, allNA = xvar, min_val = NULL ) {
    
    n_valid <- sum( ! is.na(xvar))  #count the number of non-NA's
    n <- length(xvar)
    
    #special cases
    if ( n_valid == 0 ) return(allNA)  # no non-NA's -- give up
    if ( n_valid == 1 ) return( rep( mean(xvar, na.rm=TRUE), length(xvar))) #only one value
    
    valids <- which( ! is.na( xvar ))
    d_mph <- diff( mph )
    slope <- diff( xvar[ valids ]) / diff( mph[ valids ])  # known slopes
    slope <- c( slope, slope[n_valid-1] )
    start_pt <- c( valids, 61)

    y_t <- xvar  #output vector
    
    #extrapolate first point if necessary
    if ( is.na(xvar[1]))  y_t[1] <- 
        y_t[ start_pt[1] ] -  slope[1] * ( mph[ start_pt[1]] - mph[1] )
    
    #extrapolate last point if necessary
    if ( is.na(xvar[n]))  {
        last_valid <- valids[ length(valids) ]
        y_t[n] <- y_t[ last_valid ] +  slope[n_valid-1] * ( mph[ n] - mph[ last_valid] )
    }
    
    iseg <- 1
    for ( i in 2:(n-1)) {
        if (i < start_pt[iseg + 1] ) {
            y_t[i] <- slope[iseg] * d_mph[i] + y_t[i-1]
        } else {
            iseg <- iseg + 1
        }
    }
    
    return(y_t)
}
# data.frame( xvar, mph, y_t)

data <- test
par( mar=c(2,2,2,1))
test_vv <- function( id = sample( unique(data$Id), 1), show_plot=TRUE ) {
    print(id)
    qd <- data[ Id == id, .( xvar=Ref, mph=minutes_past) ]
    qd$imputed_var <- vimpute_var( qd$xvar, qd$mph )
    print(qd)
    
    if( show_plot ) {
        if ( all(is.na(qd$xvar))) qd$imputed_var = 0 
        
        plot( c(0,60), c( min( 0, min(qd$imputed_var)), max(qd$imputed_var))
              , type="n", ylab="Var", xlab="minutes_past", main = sprintf("Id = %d", id)) 
        
        with(qd, points( mph, imputed_var) )
        with(qd, points( mph, xvar, pch=16) )
        with(qd, lines( mph, imputed_var, col="red") )
    }
}
# test_vv(id)
    
na_counts <- data[, .( n = .N, n_NA = sum(is.na(Ref))), Id]
case_1 <- na_counts[ (n-n_NA) == 1, ]$Id

id <- 666296 # Ref: 5/8 NA  t0=NA
id <- 525038 # Ref: 6/14 NA t0=NA tn=NA
id <- 114141 # Ref: 12/14 NA t0=NA tn=NA
id=365923


xvar <- 0
mph <- 0
setup_dbg <- function( id ) {
    qd <- data[ Id == id, .( xvar=Ref, mph=minutes_past) ]
    xvar <<- qd$xvar
    mph <<- qd$mph
}
# setup_dbg( id )

set.seed(6841)
run12 <- function() {
    par( mfrow=c(4,3) )
    for( i in 1:12 ) test_vv()
    par( mfrow=c(1,1) )
}



