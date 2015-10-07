library(data.table)
library(Metrics)
library(dplyr)

train <- fread("../train.csv")
summary(train)

train[, median(Expected)]
t0b <- proc.time()
train[,mean(Expected),Id][,median(V1)]
t1b <- proc.time()
t1b-t0b

#mae <- function( y, yhat) mean( abs( y-yhat ))

train.df <- tbl_df(train)
t0 <- proc.time()
train %>% group_by(Id) %>% summarise( V1 = mean(Expected)) %>% summarise( median(V1))
t1 <- proc.time()
t1-t0

xx <- train %>% 
    select( starts_with("Ref"), starts_with("Rho"), starts_with("Zdr"), starts_with("Kdp") ) %>%
    transmute( allNA = all(is.na(Ref), is.na(Kdp)))
transmute( allNA = any(! is.na(Ref)))
