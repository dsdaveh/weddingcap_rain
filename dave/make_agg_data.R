library(data.table)
library(dplyr)

source( "../team/rain_utils.R") 

if (!exists("rain_data")) {
    cat("ERROR: variable 'rain_data' is not set, load an imputed dataset and copy to rain_data
            EG:    load('../train_imputed.RData')
                   rain_data <- train
                   source(<this file>)
            ")
    stop(  "rain_data not set" )
}
setkey( rain_data, Id )

v_agg <- function( xvar, mph, allNA = mean(xvar), fun=identity ) {
    if ( all( is.na(xvar ))) return (allNA)
    sum( fun(  (xvar[-1] + xvar[-length(xvar)] ) /2 ) * diff( mph )/60  )
}

rain_agg <- rain_data[, .( rd = max(radardist_km)
    , Ref = v_agg( Ref, minutes_past)
    , RefComposite = v_agg( RefComposite, minutes_past, -999)
    , RhoHV = v_agg( RhoHV, minutes_past, -999)
    , Zdr = v_agg( Zdr, minutes_past, -999)
    , Kdp = v_agg( Kdp, minutes_past, -999)
    , nrec = .N
    , naRef = sum( is.na(Ref))
    , naRefC = sum( is.na(RefComposite))
    , naRho = sum( is.na(RhoHV))
    , naZdr = sum( is.na(Zdr))
    , Ref_rz = v_agg( Ref, minutes_past, -999, ref_to_mm)
    , Kdp_rk = v_agg( Kdp, minutes_past, -999, kdp_to_mm)
), Id ]                                                  ;tcheck()

if ( any( grepl( "^Expected$", colnames(rain_data)))) {
    rain_agg$Expected <- rain_data[, max(Expected), Id]$V1
} 
    




