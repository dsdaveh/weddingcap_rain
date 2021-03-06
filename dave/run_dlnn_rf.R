library (ggvis)
library (tidyr)


# rdata_file <- 'train_agg_10pct-mod.RData'
# load( '../train_agg2.RData' )
rdata_file <- '../train_agg-mod.RData'
# rtest_file <- gsub("^train", "test", rdata_file)
rtest_file <- '../test_agg-mod.RData'

run_id_pref <- 'dlnn_f07b_brazil2'
h2o_script <- '../dave/h2o_dlnn_cv.R'
create_submission <- TRUE
cv_frac_trn <- .7
tcheck.print <- TRUE
set_rain_thresh <- 65

mae_res <- data.frame()
run_time <- numeric() 

set_cs <- c("rd"
            , "Ref", "Ref_5x5_50th", "Ref_5x5_90th"
            , "RefComposite", "RefComposite_5x5_50th", "RefComposite_5x5_90th"
            , "Zdr", "Zdr_5x5_50th", "Zdr_5x5_90th"
            , "nrec", "naRef" 
            , "Ref_rz", "Kdp", "Kdp_rk", "rr_Katsumata_ref", "rr_refzdr", "rr_kdpzdr"
)
cs_list <- list(   kaggle = set_cs )

#for (set_seed in c(99)) { #} c(1999, 2015, 7, 86, 99)) {
set_seed <- 99
    run_id <- run_id_pref # paste( run_id_pref, set_seed, sep="_")
    mae_base <- -1 
    for (i in 1:length(cs_list)) {
        print( cs_list[i])
        set_cs <- cs_list[[i]]
        source (h2o_script)
        elapsed <- sum( time_df$delta )
        mae_base <- ifelse( mae_base > 0 , mae_base, mae_cv_test)
        mae_res <- rbind( mae_res, data.frame( seed=set_seed, xSet=names(cs_list[i])
                                               , xvars=paste(set_cs, collapse = ",")
                                               , mae_scrub_trn, mae_cv_test, mae_cv_trn
                                               , delta = mae_cv_test - mae_base
                                               , elapsed))
        run_time <- c(run_time, elapsed )
    }
#}

print(mae_res)

# mae_res %>% group_by(seed) %>% 
#     ggvis( ~xSet, ~delta) %>% layer_points(fill = ~as.factor(seed))

#mae_res %>% filter( xSet == 'rf_mpk') %>% ggvis( ~as.character(seed), ~delta) %>% layer_points()
    
# mae_res %>% group_by(seed) %>% 
#     ggvis( ~rain_thresh, ~mae_cv_test ) %>%
#     layer_points( fill = ~as.factor(seed)) %>%
#     layer_lines ( stroke = ~as.factor(seed)) 