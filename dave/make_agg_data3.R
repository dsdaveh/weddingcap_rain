# based on make_agg_data.R
# adds variables used by the H2O RF kaggle script
# SEM:  adds all variables

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


rain_agg <- rain_data[, .( rd = max(radardist_km)
    , Ref = v_agg( Ref, minutes_past)
    , Ref_5x5_10th = v_agg( Ref_5x5_10th, minutes_past, -999)
    , Ref_5x5_50th = v_agg( Ref_5x5_50th, minutes_past, -999)
    , Ref_5x5_90th = v_agg( Ref_5x5_90th, minutes_past, -999)
    , RefComposite = v_agg( RefComposite, minutes_past, -999)
    , RefComposite_5x5_10th = v_agg( RefComposite_5x5_10th, minutes_past, -999)
    , RefComposite_5x5_50th = v_agg( RefComposite_5x5_50th, minutes_past, -999)
    , RefComposite_5x5_90th = v_agg( RefComposite_5x5_90th, minutes_past, -999)
    , RhoHV = v_agg( RhoHV, minutes_past, -999)
    , RhoHV_5x5_10th = v_agg( RhoHV_5x5_10th, minutes_past, -999)
    , RhoHV_5x5_50th = v_agg( RhoHV_5x5_50th, minutes_past, -999)
    , RhoHV_5x5_90th = v_agg( RhoHV_5x5_90th, minutes_past, -999)
    , Zdr = v_agg( Zdr, minutes_past, -999)
    , Zdr_5x5_10th = v_agg( Zdr_5x5_10th, minutes_past, -999)
    , Zdr_5x5_50th = v_agg( Zdr_5x5_50th, minutes_past, -999)
    , Zdr_5x5_90th = v_agg( Zdr_5x5_90th, minutes_past, -999)
    , Kdp = v_agg( Kdp, minutes_past, -999)
    , Kdp_5x5_10th = v_agg( Kdp_5x5_10th, minutes_past, -999)
    , Kdp_5x5_50th = v_agg( Kdp_5x5_50th, minutes_past, -999)
    , Kdp_5x5_90th = v_agg( Kdp_5x5_90th, minutes_past, -999)
    , nrec = .N
    , naRef = sum( naRef )
    , naRefC = sum( naRefC )
    , naRho = sum( naRho )
    , naZdr = sum( naZdr )
    , naKdp = sum( naKdp )
    , Ref_rz = v_agg( Ref, minutes_past, -999, ref_to_mm)
    , Ref_rz_comp = v_agg( RefComposite, minutes_past, -999, ref_to_mm)
    , Kdp_rk = v_agg( Kdp, minutes_past, -999, kdp_to_mm)
    , rr_Katsumata_ref = v_agg( Ref, minutes_past, -999, katsumata_ref_to_mm)
    , rr_Katsumata_ref_comp = v_agg( RefComposite, minutes_past, -999, katsumata_ref_to_mm)
    , rr_refzdr = v_agg2(Ref, Zdr, minutes_past, -999, refzdr_to_mm)
    , rr_refzdr_comp = v_agg2(RefComposite, Zdr, minutes_past, -999, refzdr_to_mm)
    , rr_kdpzdr = v_agg2(Kdp, Zdr, minutes_past, -999, kdpzdr_to_mm)
), Id ]                                                  ;tcheck()

rain_agg$Ref2 <- rain_agg$Ref^2
rain_agg$RefComposite2 <- rain_agg$RefComposite^2
rain_agg$Zdr2 <- rain_agg$Zdr^2
rain_agg$Kdp2 <- rain_agg$Kdp^2
rain_agg$rd_Ref <- rain_agg$rd*rain_agg$Ref
rain_agg$rd_RefComposite <- rain_agg$rd*rain_agg$RefComposite
rain_agg$rd_Kdp <- rain_agg$rd*rain_agg$Kdp

if ( any( grepl( "^Expected$", colnames(rain_data)))) {
    rain_agg$Expected <- rain_data[, max(Expected), Id]$V1
} 
    




