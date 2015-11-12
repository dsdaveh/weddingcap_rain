library (ggvis)
library (tidyr)
rdata_file <- '../train_agg_10pct.RData'

create_submission <- TRUE
cv_frac_trn <- .7
tcheck.print <- TRUE

mae_res <- data.frame()
run_time <- numeric() 


for (set_seed in c(1999)) {   #}, 2015, 7)) {
    for (set_rain_thresh in seq(10, 100, 10)) {
        set_rain_thresh_lower <- set_rain_thresh - 10
        run_id <- sprintf("gbm_slice_%d_%d", set_rain_thresh_lower, set_rain_thresh)
        source ('../dave/gbm_cv.R')
        mae_res <- rbind( mae_res, data.frame( seed=set_seed, rain_thresh=set_rain_thresh, mae_cv_test, mae_cv_trn, mae_xgb))
        run_time <- c(run_time, sum( time_df$delta ))
    }
}

mae_res %>% group_by(seed) %>% 
    ggvis( ~rain_thresh, ~mae_cv_test ) %>%
    layer_points( fill = ~as.factor(seed)) %>%
    layer_lines ( stroke = ~as.factor(seed)) 
    
