library(ggvis)

rain_threshold <- seq(10,70,2)
mae_df <- data.frame()
for (run_seed in c(99,1999)) {
    mae_ensemble <- numeric()
    for ( set_rain_thresh in rain_threshold) {
        cat (sprintf(".%s.", set_rain_thresh))
        source( 'lasso_ensemble.R')
        mae_ensemble <- c(mae_ensemble, mae(ensemble.frame$y, ensemble.frame$final))
    }
    mae_df <- rbind(mae_df, data.frame( mae_ensemble = mae_ensemble
                                        , rain_threshold = rain_threshold
                                        , seed = run_seed) )
}

mae_df %>% group_by( seed ) %>% 
    ggvis( ~rain_threshold, ~mae_ensemble) %>%
    layer_points( fill=~as.factor(seed)) %>%
    layer_lines( stroke = ~as.factor(seed))

