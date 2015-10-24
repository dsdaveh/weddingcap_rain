library(dplyr)
library(data.table)
library(Metrics)
library(ggvis)
library(xtable)
library(Hmisc)

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
    , yhat2 = mpalmer(RefComposite, minutes_past )
    , y = max(Expected)
), Id][ yhat >= 0, ][ y > .255 & y < 40 ][ , Refunmp := unmp( yhat )]

rf$abs_err  <- abs( rf$yhat - rf$y )
rf$abs_err2 <- abs( rf$yhat2 - rf$y )

par.orig <- par( mfrow=c(3,1), mar = c(2, 4, 2, 4))

with( rf[ Id %% 4 == 0] , plot( Refunmp, y, type = "p") )

rf$refgrp <- as.numeric(cut2( rf$Refunmp, seq(-10,40,10)))
rf$refgrp2 <- cut2( rf$Refunmp, seq(-10,40,10))

boxplot( rf$y ~ rf$refgrp2)
plot( -10:40, mphr(-10:40))
par( par.orig)

setkey( rf, y)

ref_desc <- rf[, .( N = .N
    , mean=mean(y)
    , med=median(y)
    , med_hat = median(yhat)
    , med_hat2 = median(yhat2)
    , q1=quantile(y, probs=.25)
    , q3=quantile(y, probs=.75)
    , mae = mae( yhat, y)
    , mae2 = mae( yhat2, y)
    ), refgrp2][, whisker := (q3-q1)*1.5 + q3]

setkey( ref_desc, refgrp2)

Ref_binx <- seq(-15, 45, 10)
plot( Ref_binx, mphr(Ref_binx), type="b")
with( ref_desc[ !is.na(refgrp2), ], lines( Ref_binx, med, col= "red"))
with( ref_desc[ !is.na(refgrp2), ], lines( Ref_binx, med_hat, col= "blue", lty=2))
with( ref_desc[ !is.na(refgrp2), ], lines( Ref_binx, med_hat2, col= "blue", lty=3))
with( ref_desc[ !is.na(refgrp2), ], lines( Ref_binx, mae, col= "magenta", lty=2))
with( ref_desc[ !is.na(refgrp2), ], lines( Ref_binx, mae2, col= "magenta", lty=3))
legend( "topleft", legend=c("M-Palmer Theory", "Measured", "M-P(Ref)", "M-P(RefComposite)", "MAE(Ref)", "MAE(RefComposite)")
        , col=c("black", "red", "blue", "blue", "magenta", "magenta"), lty=c(1,1,2,3,2,3))



