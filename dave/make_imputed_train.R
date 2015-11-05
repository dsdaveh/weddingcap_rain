##Build an imputed dataset with only the Ref set

library(data.table)
library(dplyr)

source( "../team/rain_utils.R")  ; tcheck(desc = "load dataset")

rdata_file <- "train_10pct.RData"
rdata_file <- "train_full.RData"
load( rdata_file )
setkey(train, Id, minutes_past)

tcheck(0)

impute_steps_train <- data.frame( Id = unique(train$Id)) %>% arrange( Id ) %>% tbl_df

#Id's with first = 0  
has_t0 <- train[ minutes_past == 0, ]$Id
add_t0  <- train[ ! Id %in% has_t0, .SD[1],  Id]
add_t60 <- train[                 , .SD[.N], Id]   #no Id's have last = 60

impute_steps_train$add_t0 <- ! impute_steps_train$Id %in% has_t0

train <- train %>% 
    bind_rows( add_t0 ) %>% 
    bind_rows( add_t60) %>%
    data.table();                           tcheck(desc = "add t0 and t60")

setkey(train, Id, minutes_past)

train$Ref <- train[, .( impute = vimpute_var( Ref, minutes_past, method=2)), Id]$impute ; tcheck(desc="impute Ref")
train$Ref_5x5_10th <- train[, .( impute = vimpute_var( Ref_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Ref_5x5_50th <- train[, .( impute = vimpute_var( Ref_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Ref_5x5_90th <- train[, .( impute = vimpute_var( Ref_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RefComposite <- train[, .( impute = vimpute_var( RefComposite, minutes_past, method=2)), Id]$impute ; tcheck()
train$RefComposite_5x5_10th <- train[, .( impute = vimpute_var( RefComposite_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RefComposite_5x5_50th <- train[, .( impute = vimpute_var( RefComposite_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RefComposite_5x5_90th <- train[, .( impute = vimpute_var( RefComposite_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RhoHV <- train[, .( impute = vimpute_var( RhoHV, minutes_past, method=2)), Id]$impute ; tcheck()
train$RhoHV_5x5_10th <- train[, .( impute = vimpute_var( RhoHV_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RhoHV_5x5_50th <- train[, .( impute = vimpute_var( RhoHV_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
train$RhoHV_5x5_90th <- train[, .( impute = vimpute_var( RhoHV_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Zdr <- train[, .( impute = vimpute_var( Zdr, minutes_past, method=2)), Id]$impute ; tcheck()
train$Zdr_5x5_10th <- train[, .( impute = vimpute_var( Zdr_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Zdr_5x5_50th <- train[, .( impute = vimpute_var( Zdr_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Zdr_5x5_90th <- train[, .( impute = vimpute_var( Zdr_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Kdp <- train[, .( impute = vimpute_var( Kdp, minutes_past, method=2)), Id]$impute ; tcheck()
train$Kdp_5x5_10th <- train[, .( impute = vimpute_var( Kdp_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Kdp_5x5_50th <- train[, .( impute = vimpute_var( Kdp_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
train$Kdp_5x5_90th <- train[, .( impute = vimpute_var( Kdp_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()

