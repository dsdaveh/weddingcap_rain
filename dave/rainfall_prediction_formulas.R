library(data.table)
library(dplyr)
library(Metrics)

source( "../team/rain_utils.R") 

# MAEs of rainfall prediction formulas

# the following is generated by dave/make_imputed_train.R
load('../train_imputed.RData')

kaggle_mpalmer <- fread("../KS_mpalmer-train.csv") %>% rename ( kaggle_mp = Expected)

rain_calcs <- train[, .( Expected = max(Expected)
                         , noRef =  all( is.na( Ref ))
                         , Ref_rz = v_agg( Ref, minutes_past, allNA=0, fun=ref_to_mm)
                         , Kdp_rk = v_agg( Kdp, minutes_past, allNA=0, fun=kdp_to_mm)
                         , rr_Katsumata_ref = v_agg( Ref, minutes_past, allNA=0, fun=katsumata_ref_to_mm)
                         , rr_refzdr = v_agg2(Ref, Zdr, minutes_past, allNA=0, fun=refzdr_to_mm)
                         , rr_kdpzdr = v_agg2(Kdp, Zdr, minutes_past, allNA=0, fun=kdpzdr_to_mm)
), Id ] %>%
    left_join( kaggle_mpalmer, by="Id")

save(rain_calcs, file="../rainfall_calculations.RData")

# replace NaN with median Expected for those values
rain_calcs[ is.nan(rr_refzdr), rr_refzdr := median(Expected)]
rain_calcs[ is.nan(rr_kdpzdr), rr_kdpzdr := median(Expected)]

# output MAEs of the rainfall prediction formulas
with( rain_calcs[ noRef == FALSE, ], {
    mae(Expected, kaggle_mp) # 24.4304
    mae(Expected, rr_Katsumata_ref) # 23.76555
    mae(Expected, rr_refzdr) # 2.394776e+78
    mae(Expected, rr_kdpzdr) # 1.105153e+47
    mae(Expected, Kdp_rk) # 58.3432
    mae(Expected, Ref_rz) # 23.37673
})

#repeat for test
load('../test_imputed.RData')

kaggle_mpalmer <- fread("../KS_mpalmer-test.csv") %>% rename ( kaggle_mp = Expected)

rain_calcs <- test[, .( noRef =  all( is.na( Ref ))
                         , Ref_rz = v_agg( Ref, minutes_past, allNA=0, fun=ref_to_mm)
                         , Kdp_rk = v_agg( Kdp, minutes_past, allNA=0, fun=kdp_to_mm)
                         , rr_Katsumata_ref = v_agg( Ref, minutes_past, allNA=0, fun=katsumata_ref_to_mm)
                         , rr_refzdr = v_agg2(Ref, Zdr, minutes_past, allNA=0, fun=refzdr_to_mm)
                         , rr_kdpzdr = v_agg2(Kdp, Zdr, minutes_past, allNA=0, fun=kdpzdr_to_mm)
), Id ] %>%
    left_join( kaggle_mpalmer, by="Id")

save(rain_calcs, file="../rainfall_calculations-test.RData")


