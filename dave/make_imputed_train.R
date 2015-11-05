##Build an imputed dataset with only the Ref set

library(data.table)
library(dplyr)

source( "../team/rain_utils.R")  ; tcheck(desc = "load dataset")

rdata_file <- "train_full.RData"
load( rdata_file )
setkey(train, Id, minutes_past)

tcheck(0)

impute_steps_train <- data.frame( Id = unique(train$Id)) %>% arrange( Id ) %>% tbl_df

#Id's with first = 0  
has_t0 <- train[ minutes_past == 0, ]$Id
add_t0 <- train[ ! Id %in% has_t0, .(
    Id=max(Id), minutes_past=0, radardist_km=max(radardist_km), Ref=NA, Expected=max(Expected)
    ), Id] 

impute_steps_train$add_t0 <- ! impute_steps_train$Id %in% has_t0

#no Id's have last = 60
add_t60 <- train[ , .(
    Id=max(Id), minutes_past=60, radardist_km=max(radardist_km), Ref=NA, Expected=max(Expected)
), Id] 


train <- train %>% select( Id, minutes_past, radardist_km, Ref, Expected) %>%
    bind_rows( add_t0 ) %>% 
    bind_rows( add_t60) %>%
    data.table();                           tcheck(desc = "add t0 and t60")

setkey(train, Id, minutes_past)

train$Ref <- train[, .( impute = vimpute_var( Ref, minutes_past, method=2)), Id]$impute ; tcheck(desc="impute")

