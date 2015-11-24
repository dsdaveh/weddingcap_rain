library(data.table)
library(Metrics)
library(dplyr)

#input
model_trn <- "csv_out/xgbm_f70_seed99_99-cvtest.csv"
basel_trn <- "csv_out/KS_mpalmer-train.csv"

model <- read.csv( model_trn) %>% rename( yhat = Expected )
basel <- read.csv( basel_trn) %>% rename( basel = Expected )

model <- model %>% left_join( basel, by="Id") 
mae_p <- numeric()
x_p <- seq(0,1,.05)
for (p in x_p ) {
    blend <- model %>% transmute( Expected = model$basel * p + model$yhat * (1-p) )
    mae_p <- c(mae_p, mae( blend$Expected, model$y))
}

plot( x_p , mae_p , type="b")
x_min <- x_p[ which.min(mae_p)]
title( sprintf( "Blended MAE for xgbm and Marshall Palmer\nmin(MAE) at %4.2f = %10.6f", x_min, min(mae_p)))
points( x_min, min(mae_p), col="red", pch=15)
