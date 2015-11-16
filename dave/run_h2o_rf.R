library (ggvis)
library (tidyr)


rdata_file <- 'train_agg_10pct-mod.RData'
load( '../train_agg_10pct.RData' )
train_agg <- train_agg %>%
    mutate ( Ref_5x5_50th = ifelse(Ref)
test$Ref_5x5_90th[which(test$Ref_5x5_90th < 0)] <- NA
test$RefComposite[which(test$RefComposite < 0)] <- NA
test$RefComposite_5x5_50th[which(test$RefComposite_5x5_50th < 0)] <- NA
test$RefComposite_5x5_90th[which(test$RefComposite_5x5_90th < 0)] <- NA
test$Ref[which(test$Ref < 0)] <- NA
    mutate( rz_x_rd = Ref_rz * rd ) %>%
    mutate( rk_x_rd = Kdp_rk * rd ) 
save( train_agg, file=rdata_file)

create_submission <- FALSE
cv_frac_trn <- .7
tcheck.print <- TRUE
set_rain_thresh <- 9999

mae_res <- data.frame()
run_time <- numeric() 

set_cs <- c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec")
cs_list <- list( baseline = c("Ref", "RefComposite",   "Ref_rz",  "rd", "nrec") 
                  , rf_kdp = c("Ref", "RefComposite",   "Kdp", "Kdp_rk",  "rd", "nrec")
                  , rf_mpk = c("Ref", "RefComposite",   "Ref_rz", "Kdp", "Kdp_rk",  "rd", "nrec")
                 , rf_xrd = c("Ref", "RefComposite",   "Ref_rz", "Kdp", "Kdp_rk"
                              , "rz_x_rd", "rk_x_rd", "rd", "nrec")
)


for (set_seed in c(1999, 2015, 7)) {
    mae_base <- -1 
    for (i in 1:length(cs_list)) {
        print( cs_list[i])
        set_cs <- cs_list[[i]]
        source ('../dave/gbm_cv.R')
        elapsed <- sum( time_df$delta )
        mae_base <- ifelse( mae_base > 0 , mae_base, mae_cv_test)
        mae_res <- rbind( mae_res, data.frame( seed=set_seed, xSet=names(cs_list[i])
                                               , xvars=paste(set_cs, collapse = ",")
                                               , mae_cv_test, mae_cv_trn, mae_xgb
                                               , delta = mae_cv_test - mae_base
                                               , elapsed))
        run_time <- c(run_time, elapsed )
    }
}

mae_res %>% group_by(seed) %>% 
    ggvis( ~xSet, ~delta) %>% layer_points(fill = ~as.factor(seed))

#mae_res %>% filter( xSet == 'rf_mpk') %>% ggvis( ~as.character(seed), ~delta) %>% layer_points()
    
# mae_res %>% group_by(seed) %>% 
#     ggvis( ~rain_thresh, ~mae_cv_test ) %>%
#     layer_points( fill = ~as.factor(seed)) %>%
#     layer_lines ( stroke = ~as.factor(seed)) 