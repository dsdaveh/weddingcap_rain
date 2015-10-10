#naive linear model
library(MASS)

#load and prep the data if this hasn't been done
source("../team/data-prep.R")

model.all <- lm(Expected ~ minutes_past + radardist_km + Ref + Ref_5x5_10th + 
                  Ref_5x5_50th + Ref_5x5_90th + RefComposite + RefComposite_5x5_10th + 
                  RefComposite_5x5_50th + RefComposite_5x5_90th + RhoHV + RhoHV_5x5_10th + 
                  RhoHV_5x5_50th + RhoHV_5x5_90th + Zdr + Zdr_5x5_10th + Zdr_5x5_50th + 
                  Zdr_5x5_90th + Kdp + Kdp_5x5_10th + Kdp_5x5_50th + Kdp_5x5_90th, data=train.sample1000)
step.bwd <- stepAIC(model.all, direction = "backward")
plot.residuals(step.bwd, train.sample1000)

step.fwd <- stepAIC(model.all, direction = "forward")
model.res <- resid(step.fwd)
plot.residuals(step.fwd, train.sample1000)

train.sample1000.naomit <- na.omit(train.sample1000)
jpeg("../scott/plots/lm-minutes-past-res.jpg")
plot(train.sample1000.naomit$minutes_past, model.res, ylab="Residuals",
     xlab="minutes_past", main="res") 
abline(0, 0)
dev.off()



###
# function to plot residuals for a model
###
plot.residuals <- function(lm, df) {
  df.naomit <- na.omit(df)
  model.res = resid(lm)
  par(mfrow=c(8,3))
  plot(df.naomit$minutes_past, model.res, ylab="Residuals", xlab="minutes_past", main="Residual Plot") 
  abline(0, 0)
  plot(df.naomit$radardist_km, model.res, ylab="Residuals", xlab="radardist_km", main="Residual Plot")
  plot(df.naomit$Ref, model.res, ylab="Residuals", xlab="Ref", main="Residual Plot")
  plot(df.naomit$Ref_5x5_10th, model.res, ylab="Residuals", xlab="Ref_5x5_10th", main="Residual Plot")
  plot(df.naomit$Ref_5x5_50th, model.res, ylab="Residuals", xlab="Ref_5x5_50th", main="Residual Plot")
  plot(df.naomit$Ref_5x5_90th, model.res, ylab="Residuals", xlab="Ref_5x5_90th", main="Residual Plot")
  plot(df.naomit$RefComposite, model.res, ylab="Residuals", xlab="RefComposite", main="Residual Plot")
  plot(df.naomit$RefComposite_5x5_10th, model.res, ylab="Residuals", xlab="RefComposite_5x5_10th", main="Residual Plot")
  plot(df.naomit$RefComposite_5x5_50th, model.res, ylab="Residuals", xlab="RefComposite_5x5_50th", main="Residual Plot")
  plot(df.naomit$RefComposite_5x5_90th, model.res, ylab="Residuals", xlab="RefComposite_5x5_90th", main="Residual Plot")
  plot(df.naomit$RhoHV, model.res, ylab="Residuals", xlab="RhoHV", main="Residual Plot")
  plot(df.naomit$RhoHV_5x5_10th, model.res, ylab="Residuals", xlab="RhoHV_5x5_10th", main="Residual Plot")
  plot(df.naomit$RhoHV_5x5_50th, model.res, ylab="Residuals", xlab="RhoHV_5x5_50th", main="Residual Plot")
  plot(df.naomit$RhoHV_5x5_90th, model.res, ylab="Residuals", xlab="RhoHV_5x5_90th", main="Residual Plot")
  plot(df.naomit$Zdr, model.res, ylab="Residuals", xlab="Zdr", main="Residual Plot")
  plot(df.naomit$Zdr_5x5_10th, model.res, ylab="Residuals", xlab="Zdr_5x5_10th", main="Residual Plot")
  plot(df.naomit$Zdr_5x5_50th, model.res, ylab="Residuals", xlab="Zdr_5x5_50th", main="Residual Plot")
  plot(df.naomit$Zdr_5x5_90th, model.res, ylab="Residuals", xlab="Zdr_5x5_90th", main="Residual Plot")
  plot(df.naomit$Kdp, model.res, ylab="Residuals", xlab="Kdp", main="Residual Plot")
  plot(df.naomit$Kdp_5x5_10th, model.res, ylab="Residuals", xlab="Kdp_5x5_10th", main="Residual Plot")
  plot(df.naomit$Kdp_5x5_50th, model.res, ylab="Residuals", xlab="Kdp_5x5_50th", main="Residual Plot")
  plot(df.naomit$Kdp_5x5_90th, model.res, ylab="Residuals", xlab="Kdp_5x5_90th", main="Residual Plot")
  return(model.res)
}
