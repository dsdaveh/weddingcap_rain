rdata_file <- 'train_agg-mod.RData'
rtest_file <- 'test_agg-mod.RData'

load( '../train_agg2.RData' )
load( '../test_agg2.RData' )

set_small_to_na <- function( df ) {
    with( df, {
        df$Ref <- ifelse( (Ref != -999) & (Ref < 0), NA, Ref )
        df$Ref_5x5_50th <- ifelse( (Ref_5x5_50th != -999) & (Ref_5x5_50th < 0), NA, Ref_5x5_50th )
        df$Ref_5x5_90th <- ifelse( (Ref_5x5_90th != -999) & (Ref_5x5_90th < 0), NA, Ref_5x5_90th )
        df$RefComposite <- ifelse( (RefComposite != -999) & (RefComposite < 0), NA, RefComposite )
        df$RefComposite_5x5_50th <- ifelse( (RefComposite_5x5_50th != -999) & (RefComposite_5x5_50th < 0), NA, RefComposite_5x5_50th )
        df$RefComposite_5x5_90th <- ifelse( (RefComposite_5x5_90th != -999) & (RefComposite_5x5_90th < 0), NA, RefComposite_5x5_90th )
    })
    return(df)
}
train_agg <- set_small_to_na(train_agg) 
test_agg <- set_small_to_na(test_agg) 
save( train_agg, file=rdata_file)
save( test_agg, file=rtest_file)
