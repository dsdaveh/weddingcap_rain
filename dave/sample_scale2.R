source("../team/data-prep.R")
source( "KS_mpalmer.R")

library(ggvis)
results %>% ggvis( ~Expected ) %>% layer_histograms( width = 2)
results %>% ggvis( ~log(Expected+1)) %>% layer_histograms( )

#dynamic scal factor
# thinking is we don't want to scale 163mm , but general predications are too low
MaxExpect <- max(results$Expected)

results <- results %>%
    mutate( scaled = Expected * ( -Expected * 2 / MaxExpect  +2 ) ) %>%
    select( Id, Expected = scaled)
    
write.csv(results, file='KS_mpalmer_scaled.csv', row.names=FALSE)

