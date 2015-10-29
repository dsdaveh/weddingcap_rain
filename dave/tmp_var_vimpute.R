library(data.table)

source("../team/rain_utils")
# data.frame( xvar, mph, y_t)

data <- test
par( mar=c(2,2,2,1))
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



