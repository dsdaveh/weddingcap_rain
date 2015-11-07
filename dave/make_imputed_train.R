if (! exists("silent_flag") ) tcheck.print <- TRUE  # set to false to see summary only - true prints during execution

library(data.table)
library(dplyr)

source( "../team/rain_utils.R")  ; tcheck(desc = "load dataset")

cat("loading train from CSV\n")
train <- fread("../train.csv")  ; tcheck(desc = "load dataset from csv")

setkey(train, Id, minutes_past)

tcheck(0)

impute_steps_train <- data.frame( Id = unique(train$Id)) %>% arrange( Id ) %>% tbl_df

#Id's with first = 0  
has_t0 <- train[ minutes_past == 0, ]$Id
add_t0  <- train[ ! Id %in% has_t0, .SD[1],  Id]
add_t60 <- train[                 , .SD[.N], Id]   #no Id's have last = 60
add_t0$minutes_past  <- 0
add_t60$minutes_past <- 60

impute_steps_train$add_t0 <- ! impute_steps_train$Id %in% has_t0

train <- rbind( train, add_t0, add_t60);   tcheck(desc = "add t0 and t60")

setkey(train, Id, minutes_past)

na_recs <- train[, .( 
      naRef  = is.na(Ref)
    , naRefC = is.na(RefComposite)
    , naRho  = is.na(RhoHV)
    , naZdr  = is.na(Zdr)
    , naKdp  = is.na(Kdp)
    )] ; tcheck(desc="record NA cells")

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

train <- cbind( train, na_recs )
save(train,file="../train_imputed.RData") ; tcheck(desc="save ../train_imputed.RData")

set.seed(1999)
id10 <- sample( unique(train$Id), round( length( unique(train$Id)) / 10))
train <- train[ Id %in% id10, ]
save(train,file="../train_imputed_10pct.RData") ; tcheck(desc="save ../train_imputed_10pct.RData")

rm(train, na_recs);gc(); tcheck(desc="purge train")

tdf <- get_tcheck()
print( tdf )

train_time <- sum( tdf$delta)
print( train_time )

############################# complete repeat for test ... not using functions to save memory
cat("loading test from CSV\n")
test <- fread("../test.csv")  ; tcheck(desc = "load dataset from csv")

setkey(test, Id, minutes_past)

tcheck(0)

impute_steps_test <- data.frame( Id = unique(test$Id)) %>% arrange( Id ) %>% tbl_df

#Id's with first = 0  
has_t0 <- test[ minutes_past == 0, ]$Id
add_t0  <- test[ ! Id %in% has_t0, .SD[1],  Id]
add_t60 <- test[                 , .SD[.N], Id]   #no Id's have last = 60
add_t0$minutes_past  <- 0
add_t60$minutes_past <- 60

impute_steps_test$add_t0 <- ! impute_steps_test$Id %in% has_t0

test <- rbind( test, add_t0, add_t60);   tcheck(desc = "add t0 and t60")

setkey(test, Id, minutes_past)

na_recs <- test[, .( 
    naRef  = is.na(Ref)
    , naRefC = is.na(RefComposite)
    , naRho  = is.na(RhoHV)
    , naZdr  = is.na(Zdr)
    , naKdp  = is.na(Kdp)
)] ; tcheck(desc="record NA cells")

test$Ref <- test[, .( impute = vimpute_var( Ref, minutes_past, method=2)), Id]$impute ; tcheck(desc="impute Ref")
test$Ref_5x5_10th <- test[, .( impute = vimpute_var( Ref_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Ref_5x5_50th <- test[, .( impute = vimpute_var( Ref_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Ref_5x5_90th <- test[, .( impute = vimpute_var( Ref_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RefComposite <- test[, .( impute = vimpute_var( RefComposite, minutes_past, method=2)), Id]$impute ; tcheck()
test$RefComposite_5x5_10th <- test[, .( impute = vimpute_var( RefComposite_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RefComposite_5x5_50th <- test[, .( impute = vimpute_var( RefComposite_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RefComposite_5x5_90th <- test[, .( impute = vimpute_var( RefComposite_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RhoHV <- test[, .( impute = vimpute_var( RhoHV, minutes_past, method=2)), Id]$impute ; tcheck()
test$RhoHV_5x5_10th <- test[, .( impute = vimpute_var( RhoHV_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RhoHV_5x5_50th <- test[, .( impute = vimpute_var( RhoHV_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
test$RhoHV_5x5_90th <- test[, .( impute = vimpute_var( RhoHV_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Zdr <- test[, .( impute = vimpute_var( Zdr, minutes_past, method=2)), Id]$impute ; tcheck()
test$Zdr_5x5_10th <- test[, .( impute = vimpute_var( Zdr_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Zdr_5x5_50th <- test[, .( impute = vimpute_var( Zdr_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Zdr_5x5_90th <- test[, .( impute = vimpute_var( Zdr_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Kdp <- test[, .( impute = vimpute_var( Kdp, minutes_past, method=2)), Id]$impute ; tcheck()
test$Kdp_5x5_10th <- test[, .( impute = vimpute_var( Kdp_5x5_10th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Kdp_5x5_50th <- test[, .( impute = vimpute_var( Kdp_5x5_50th, minutes_past, method=2)), Id]$impute ; tcheck()
test$Kdp_5x5_90th <- test[, .( impute = vimpute_var( Kdp_5x5_90th, minutes_past, method=2)), Id]$impute ; tcheck()

test <- cbind( test )
save(test,file="../test_imputed.RData") ; tcheck(desc="save ../test_imputed.RData")

rm(test);gc(); tcheck(desc="purge test")

tdf <- get_tcheck()
print( tdf )

test_time <- sum( tdf$delta)
print( test_time )
print( test_time + train_time)

rm( add_t0, add_t60, test_time, train_time, has_t0, id10, na_recs); gc()



