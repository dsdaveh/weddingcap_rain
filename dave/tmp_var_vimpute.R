library(data.table)

source("../team/rain_utils.R")
source("../team/data-prep.R")

data <- train.sample1000
par( mar=c(2,2,2,1))
test_vv <- function( id = sample( unique(data$Id), 1), show_plot=TRUE, extend=TRUE, vname = "Ref" ) {
    print(id)
    if (vname == "Ref" ) qd <- data[ Id == id, .( xvar=Ref, mph=minutes_past) ]
    if (vname == "Kdp" ) qd <- data[ Id == id, .( xvar=Kdp, mph=minutes_past) ]
    print(qd)
    
    if (extend) {
        qd_orig <- qd
        qd <- extend_var_pair( qd )
        if ( ! identical( qd, qd_orig ))   cat ("extrapolated end points\n")
    }
    
    qd$imputed_m1 <- vimpute_var( qd$xvar, qd$mph, method=1 )
    qd$imputed_m2 <- vimpute_var( qd$xvar, qd$mph, method=2 )
    print(qd)
    
    if( show_plot ) {
        if ( all(is.na(qd$xvar))) qd$imputed_m1 <- qd$imputed_m2 <- 0 
        
        plot( c(0,60), c( min( 0, min(qd$imputed_m1)), max(qd$imputed_m1))
              , type="n", ylab="Var", xlab="minutes_past"
              , main = sprintf("Id = %d, vname=%s", id, vname)) 
        
        with(qd, points( mph, imputed_m1) )
        with(qd, points( mph, xvar, pch=16) )
        with(qd, lines( mph, imputed_m1, col="red") )
        with(qd, lines( mph, imputed_m2, col="red", lty=2))
        with(qd, abline( h = vimpute_agg( qd$xvar, qd$mph, method=1 ), col= "blue", lty=2) )
        with(qd, abline( h = vimpute_agg( qd$xvar, qd$mph, method=2 ), col= "blue", lty=3) )
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

run12 <- function( extend=FALSE, ... ) {
    par( mfrow=c(4,3) )
    for( i in 1:12 ) test_vv( extend=extend, ... )
    par( mfrow=c(1,1) )
}

set.seed(6841)
# run12( extend = FALSE)
set.seed(6841)
# run12( extend = TRUE ) 

# run12( extend = TRUE, vname="Kdp")