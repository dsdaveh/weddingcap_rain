rdata_file <- '../train_agg3_10pct.RData'

solver_script <- '../dave/gbm_cv.R'
create_submission <- FALSE
cv_frac_trn <- 0.7
tcheck.print <- TRUE
set_rain_thresh <- 69
set_rm_refna <- TRUE
set_nrounds <- 1955
set_maxdepth <- 3

mae_res <- data.frame()
run_time <- numeric() 

set_cs <- c("rd"
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

smallestError <- 100
for (depth in seq(1,5,1)) {
  for (rounds in seq(1,5,1)) {
    
    # train
    
    source (solver_script)
    elapsed <- sum( time_df$delta )
    mae_base <- ifelse( mae_base > 0 , mae_base, mae_cv_test)
    mae_res <- rbind( mae_res, data.frame( depth=depth, rounds=rounds, xSet=names(cs_list[i])
                                           , xvars=paste(set_cs, collapse = ",")
                                           , mae_xgb, mae_cv_test, mae_cv_trn
                                           , delta = mae_cv_test - mae_base
                                           , elapsed))
    run_time <- c(run_time, elapsed )
    
    # predict
    
    if (mae_cv_test < smallestError) {
      smallestError = mae_cv_test
      print(paste("New Lowest MAE: ", depth,rounds,mae_cv_test))
    }     
  }
}  

print(mae_res)
print(paste("Overall Lowest MAE on Test: ", depth,rounds,mae_cv_test))


