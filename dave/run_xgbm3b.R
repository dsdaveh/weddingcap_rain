# scrambling run ... against new agg3
# same as xgbm3b, run with f70  with and without set_rm_refna

library (ggvis)
library (tidyr)
library(Ckmeans.1d.dp)


rdata_file <- '../train_agg3.RData'
rtest_file <- '../test_agg3.RData'

run_id_pref <- 'csv_out/xgbm3_f70_sat1b'
solver_script <- '../dave/gbm_cv.R'
create_submission <- TRUE
cv_frac_trn <- .7
tcheck.print <- TRUE
set_rain_thresh <- 69
set_rm_refna <- TRUE

mae_res <- data.frame()
run_time <- numeric() 

kaggle <- c("rd"
            , "Ref", "Ref_5x5_50th", "Ref_5x5_90th"
            , "RefComposite", "RefComposite_5x5_50th", "RefComposite_5x5_90th"
            , "Zdr", "Zdr_5x5_50th", "Zdr_5x5_90th"
            , "nrec", "naRef" 
            , "Ref_rz", "Kdp", "Kdp_rk", "rr_Katsumata_ref", "rr_refzdr", "rr_kdpzdr"
)
nonpolar <- c("rd"
            , "Ref", "Ref_5x5_50th", "Ref_5x5_90th"
            , "nrec", "naRef" 
            , "Ref_rz", "rr_Katsumata_ref"
)
all <- c("rd"
         , "Ref", "Ref_5x5_10th", "Ref_5x5_50th", "Ref_5x5_90th"
         , "RefComposite", "RefComposite_5x5_10th", "RefComposite_5x5_50th", "RefComposite_5x5_90th"
         , "RhoHV", "RhoHV_5x5_10th", "RhoHV_5x5_50th", "RhoHV_5x5_90th"
         , "Zdr", "Zdr_5x5_10th", "Zdr_5x5_50th", "Zdr_5x5_90th"
         , "Kdp", "Kdp_5x5_10th", "Kdp_5x5_50th", "Kdp_5x5_90th"
         , "nrec", "naRef", "naRefC", "naRho", "naZdr", "naKdp"
         , "Ref_rz", "Ref_rz_comp", "Kdp_rk", "rr_Katsumata_ref", "rr_Katsumata_ref_comp"
         , "rr_refzdr", "rr_refzdr_comp", "rr_kdpzdr", "Ref2", "RefComposite2", "Zdr2"
         , "Kdp2", "rd_Ref", "rd_RefComposite", "rd_Kdp"
)
norx <- c("rd"
         , "Ref", "Ref_5x5_10th", "Ref_5x5_50th", "Ref_5x5_90th"
         , "RefComposite", "RefComposite_5x5_10th", "RefComposite_5x5_50th", "RefComposite_5x5_90th"
         , "RhoHV", "RhoHV_5x5_10th", "RhoHV_5x5_50th", "RhoHV_5x5_90th"
         , "Zdr", "Zdr_5x5_10th", "Zdr_5x5_50th", "Zdr_5x5_90th"
         , "Kdp", "Kdp_5x5_10th", "Kdp_5x5_50th", "Kdp_5x5_90th"
         , "nrec", "naRef", "naRefC", "naRho", "naZdr", "naKdp"
         , "Ref_rz", "Ref_rz_comp", "Kdp_rk", "rr_Katsumata_ref", "rr_Katsumata_ref_comp"
         , "rr_refzdr", "rr_refzdr_comp", "rr_kdpzdr"
)
cs_list <- list(   norx = norx )


for (set_seed in c(99)) { #} c(1999, 2015, 7, 86, 99)) {
    run_id <- paste( run_id_pref, set_seed, sep="_")
    mae_base <- -1 
    for (i in 1:length(cs_list)) {
        print( cs_list[i])
        set_cs <- cs_list[[i]]
        source (solver_script)
        elapsed <- sum( time_df$delta )
        mae_base <- ifelse( mae_base > 0 , mae_base, mae_cv_test)
        mae_res <- rbind( mae_res, data.frame( seed=set_seed, xSet=names(cs_list[i])
                                               , xvars=paste(set_cs, collapse = ",")
                                               , mae_xgb, mae_cv_test, mae_cv_trn
                                               , delta = mae_cv_test - mae_base
                                               , elapsed))
        run_time <- c(run_time, elapsed )
    }
}

print(mae_res)
imp_mat <- xgb.importance( feature_names = cs, model=x.mod.t); tcheck()
xgb.plot.importance(imp_mat)

# mae_res %>% group_by(seed) %>% 
#     ggvis( ~xSet, ~delta) %>% layer_points(fill = ~as.factor(seed))

#mae_res %>% filter( xSet == 'rf_mpk') %>% ggvis( ~as.character(seed), ~delta) %>% layer_points()
    
# mae_res %>% group_by(seed) %>% 
#     ggvis( ~rain_thresh, ~mae_cv_test ) %>%
#     layer_points( fill = ~as.factor(seed)) %>%
#     layer_lines ( stroke = ~as.factor(seed)) 