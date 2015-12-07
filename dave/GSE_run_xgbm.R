library (ggvis)
library (tidyr)

rdata_file <- '../train_agg3.RData'
rdata10_file <- '../train_agg3_10pct.RData'
rtest_file <- '../test_agg3.RData'
#DEV ONLY
rdata_file <- '../train_agg3_10pct.RData'

run_id_pref <- 'csv_out/GSE_xgbm_f70_10pct'
solver_script <- '../dave/GSE_gbm_cvtrain.R'
create_submission <- FALSE
cv_frac_trn <- 1  
tcheck.print <- TRUE
set_rain_thresh <- 69  # would have said 70, but the highest public script...

mae_res <- data.frame()
run_time <- numeric() 

load(file = rdata10_file)
all  <- names(train_agg)[-c(1, ncol(train_agg))]
cs_list <- list(   all  = all )


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

# mae_res %>% group_by(seed) %>% 
#     ggvis( ~xSet, ~delta) %>% layer_points(fill = ~as.factor(seed))

#mae_res %>% filter( xSet == 'rf_mpk') %>% ggvis( ~as.character(seed), ~delta) %>% layer_points()
    
# mae_res %>% group_by(seed) %>% 
#     ggvis( ~rain_thresh, ~mae_cv_test ) %>%
#     layer_points( fill = ~as.factor(seed)) %>%
#     layer_lines ( stroke = ~as.factor(seed)) 