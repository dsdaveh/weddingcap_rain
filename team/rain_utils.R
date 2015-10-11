# R utilities -  
# source("../team/rain_utils.R")

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