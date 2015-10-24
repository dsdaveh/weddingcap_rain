library(dplyr)
library(data.table)
library(Metrics)
library(ggvis)
library(xtable)

source("../team/rain_utils.R")

rdata_file <- "train_full.RData" # "train_10pct.RData" # 
rdata_file <- "train_10pct.RData" # 
load( rdata_file )

set.seed(456)
id10k <- sample(unique(train$Id), 10000)
id1k <- sample(unique(train$Id), 1000)

#tcheck()

unmp <- function (r) log( (r ^ 1.6) * 200, 10) * 10  #back out Ref from M-P prediction
mphr <- function (ref) ((10^(ref/10))/200) ^ 0.625

rf <- train[ ,duration := duration(minutes_past), Id][ , .(
    Refm = sum( Ref * duration, na.rm = TRUE ) / sum( duration, na.rm = TRUE )
    , Refx = mean(Ref, na.rm = TRUE )
    , yhat = mpalmer(Ref, minutes_past)
    , y = max(Expected)
), Id][ yhat >= 0, ][ y > .255 & y < 40 ][ , Refunmp := unmp( yhat )]

rf$abs_err <- abs( rf$yhat - rf$y )

par.orig <- par( mfrow=c(2,1), mar = c(2, 4, 2, 4))
rf$refgrp <- as.numeric(cut2( rf$Refunmp, seq(-10,40,10)))
boxplot( rf$y ~ rf$refgrp)
plot( -10:40, mphr(-10:40))
par( par.orig)

setkey( rf, y)

with( rf, points( Refunmp, y, type = "p") )
