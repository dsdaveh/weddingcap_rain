library (ggvis)
library (tidyr)


# rdata_file <- 'train_agg_10pct-mod.RData'
# load( '../train_agg2.RData' )
# rdata_file <- 'train_agg-mod.RData'

#TODO generate the local test test dataset and use string manipulation for the name in h2o_rf_cv.R

# Use this code to create the modified dataset if it doesn't exist
# load( '../train_agg2.RData' )
# load( '../train_agg2_10pct.RData' )
# set_small_to_na <- function( df ) {
#     with( df, {
#         df$Ref <- ifelse( (Ref != -999) & (Ref < 0), NA, Ref )
#         df$Ref_5x5_50th <- ifelse( (Ref_5x5_50th != -999) & (Ref_5x5_50th < 0), NA, Ref_5x5_50th )
#         df$Ref_5x5_90th <- ifelse( (Ref_5x5_90th != -999) & (Ref_5x5_90th < 0), NA, Ref_5x5_90th )
#         df$RefComposite <- ifelse( (RefComposite != -999) & (RefComposite < 0), NA, RefComposite )
#         df$RefComposite_5x5_50th <- ifelse( (RefComposite_5x5_50th != -999) & (RefComposite_5x5_50th < 0), NA, RefComposite_5x5_50th )
#         df$RefComposite_5x5_90th <- ifelse( (RefComposite_5x5_90th != -999) & (RefComposite_5x5_90th < 0), NA, RefComposite_5x5_90th )
#     })
#     return(df)
# }
# train_agg <- set_small_to_na(train_agg) 
# save( train_agg, file=rdata_file)


create_submission <- FALSE
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
            , "Ref_rz", "Kdp", "Kdp_rk"
)
cs_list <- list(   kaggle = set_cs )

for (set_seed in c(1999)) { #}, 2015, 7)) {
    mae_base <- -1 
    for (i in 1:length(cs_list)) {
        print( cs_list[i])
        set_cs <- cs_list[[i]]
        source ('../dave/h2o_rf_cv.R')
        elapsed <- sum( time_df$delta )
        mae_base <- ifelse( mae_base > 0 , mae_base, mae_cv_test)
        mae_res <- rbind( mae_res, data.frame( seed=set_seed, xSet=names(cs_list[i])
                                               , xvars=paste(set_cs, collapse = ",")
                                               , mae_scrub_trn, mae_cv_test, mae_cv_trn, mae_test
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