library(data.table)
library(Metrics)
library(dplyr)

train <- fread("../train.csv")
object.size(train)
#summary(train)

train[, median(Expected)]
t0 <- proc.time()
train[,mean(Expected),Id][,median(V1)]
t1 <- proc.time()
t1-t0

#mae <- function( y, yhat) mean( abs( y-yhat ))

t0 <- proc.time()
train %>% group_by(Id) %>% 
    summarise( V1 = mean(Expected)) %>% 
    summarise( median(V1))
t1 <- proc.time()
t1-t0

object.size(train)
t0 <- proc.time()
na_obs <- train %>% 
    select( starts_with("Ref"), starts_with("Rho"), starts_with("Zdr"), starts_with("Kdp")) %>%
    .[, is.na(.SD)] %>% 
    rowSums() == 20
t1 <- proc.time()
sum(na_obs) / length(na_obs)
train <- train[ ! na_obs, ]
t2 <- proc.time()
t1-t0
t2-t1
object.size(train)

t0 <- proc.time()
train %>% group_by(Id) %>% 
    summarise( V1 = mean(Expected)) %>% 
    summarise( median(V1))
t1 <- proc.time()
t1-t0


