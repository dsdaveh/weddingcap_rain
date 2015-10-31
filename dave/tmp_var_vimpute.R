library(data.table)

source("../team/rain_utils.R")
source("../team/data-prep.R")

data <- train.sample1000
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
# test_vv(id)
    
na_counts <- data[, .( n = .N, n_NA = sum(is.na(Ref))), Id]
case_1 <- na_counts[ (n-n_NA) == 1, ]$Id

# id <- 666296 # Ref: 5/8 NA  t0=NA
# id <- 525038 # Ref: 6/14 NA t0=NA tn=NA
# id <- 114141 # Ref: 12/14 NA t0=NA tn=NA
# id=365923


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



