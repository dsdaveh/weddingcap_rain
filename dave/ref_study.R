## Use with caution....not fully QA'd yet.

library(dplyr)
library(data.table)
library(ggvis)

source("../team/rain_utils.R")

make_ref_data <- function() {
    
    
    train <- fread('../train.csv')
    results <- train %>% group_by(Id) %>% summarize(yhat=mpalmer(Ref, minutes_past))
    
    ref <- train[ , .(
            rd = mean( radardist_km, na.rm=TRUE)
            ,mean = mean(Ref, na.rm=TRUE)
            ,median = median(Ref, na.rm=TRUE)
            ,min = min(Ref, na.rm=TRUE)
            ,max = max(Ref, na.rm=TRUE)
            ,sd = sd(Ref, na.rm=TRUE)
            ,records = .N
            ,naCounts = sum(is.na(Ref))
            ,Measured = max(Expected) 
            ), Id] %>% 
        left_join(results, by="Id") %>% 
        mutate(err=yhat-Measured)
    
    save(ref, file="ref.RData")
    ref
}

if (!exists("ref")) {
    if (file.exists("ref.RData")) {
        cat("loading ref from RData file\n")
        load("ref.RData")
    } else {
        cat("loading ref from CSV\n")
        ref <- make_ref_data()
    }
}

ref_valid <- ref[ ! is.nan(mean) & (Measured < 70), ]

range(ref_valid$mean)
hist(ref_valid$mean)
sd(ref_valid$mean)
mean(ref_valid$mean)
median(ref_valid$mean)

################ Ref specific analysis

#histogram of error
# ref_valid %>% ggvis( ~err ) %>% layer_histograms()

ref_valid$lnabs_err <- log( abs( ref_valid$err))
ref_valid %>% ggvis( ~lnabs_err ) %>% layer_histograms()
sd( ref_valid$lnabs_err)
exp(3) # ~20 mm of error is 2 sigma

r5p <- sample_frac( ref_valid, size = .05)
r5p %>% ggvis(~mean, ~Measured) %>%
    layer_points( fill := 'green', opacity := 0.5)
