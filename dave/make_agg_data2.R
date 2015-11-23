# based on make_agg_data.R
# adds variables used by the H2O RF kaggle script
#

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

v_agg2 <- function( xvar, xvar2, mph, allNA = mean(xvar), fun=identity ) {
  if ( all( is.na(xvar ))) return (allNA)
  if ( all( is.na(xvar2 ))) return (allNA)
  sum( fun(  (xvar[-1] + xvar[-length(xvar)] ) /2,
             (xvar2[-1] + xvar2[-length(xvar2)] ) /2) * diff( mph )/60  )
}

rain_agg <- rain_data[, .( rd = max(radardist_km)
    , Ref = v_agg( Ref, minutes_past)
    , Ref_5x5_50th = v_agg( Ref_5x5_50th, minutes_past, -999)
    , Ref_5x5_90th = v_agg( Ref_5x5_90th, minutes_past, -999)
    , RefComposite = v_agg( RefComposite, minutes_past, -999)
    , RefComposite_5x5_50th = v_agg( RefComposite_5x5_50th, minutes_past, -999)
    , RefComposite_5x5_90th = v_agg( RefComposite_5x5_90th, minutes_past, -999)
    , RhoHV = v_agg( RhoHV, minutes_past, -999)
    , Zdr = v_agg( Zdr, minutes_past, -999)
    , Zdr_5x5_50th = v_agg( Zdr_5x5_50th, minutes_past, -999)
    , Zdr_5x5_90th = v_agg( Zdr_5x5_90th, minutes_past, -999)
    , Kdp = v_agg( Kdp, minutes_past, -999)
    , nrec = .N
    , naRef = sum( naRef )
    , naRefC = sum( naRefC )
    , naRho = sum( naRho )
    , naZdr = sum( naZdr )
    , naKdp = sum( naKdp )
    , Ref_rz = v_agg( Ref, minutes_past, -999, ref_to_mm)
    , Kdp_rk = v_agg( Kdp, minutes_past, -999, kdp_to_mm)
    , rr_Katsumata_ref = v_agg( Ref, minutes_past, -999, katsumata_ref_to_mm)
    , rr_refzdr = v_agg2(Ref, Zdr, minutes_past, -999, refzdr_to_mm)
    , rr_kdpzdr = v_agg2(Kdp, Zdr, minutes_past, -999, kdpzdr_to_mm)
), Id ]                                                  ;tcheck()

with(rain_agg, {
    rain_agg$Ref <- ifelse( Ref < 0 , NA, Ref)
    rain_agg$Ref_5x5_50th <- ifelse( Ref_5x5_50th < 0, NA, Ref_5x5_50th)
    rain_agg$Ref_5x5_90th <- ifelse( Ref_5x5_90th < 0, NA, Ref_5x5_90th)
    rain_agg$RefComposite <- ifelse( RefComposite < 0, NA, RefComposite)
    rain_agg$RefComposite_5x5_50th <- ifelse( RefComposite_5x5_50th < 0, NA, RefComposite_5x5_50th )
    rain_agg$RefComposite_5x5_90th <- ifelse( RefComposite_5x5_90th < 0, NA, RefComposite_5x5_90th )
} )
         
if ( any( grepl( "^Expected$", colnames(rain_data)))) {
    rain_agg$Expected <- rain_data[, max(Expected), Id]$V1
} 
    




