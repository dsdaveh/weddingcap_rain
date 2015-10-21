# Linear Model Selection

library(leaps)

# Best subset selection
# BIC suggests the smalletst model with 10 predictors: 

# BIC with train.complete.cases & full train data sets: 
# minutes_past + radardist_km + Ref_5x5_50th + Ref_5x5_90th + RefComposite_5x5_10th + RefComposite_5x5_50th + RhoHV_5x5_10th + RhoHV_5x5_50th + RhoHV_5x5_90th + Zdr_5x5_50th 



# Initial results seem to be the same (in terms of num of predictors) regardless of full train set or a subset (train.complete.cases)

#regfit.full=regsubsets(Expected~.,data=train.complete.cases, nvmax=23)
regfit.full=regsubsets(Expected~., data=train, nvmax=23)
regfit.fwd=regsubsets(Expected~., data=train, nvmax=23, method="forward")
regfit.bwd=regsubsets(Expected~., data=train, nvmax=23, method="backward")

reg.summary.full <- summary(regfit.full)
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.bwd <- summary(regfit.bwd)

reg.summary.full$which
reg.summary.full$rsq
reg.summary.full$rss
reg.summary$.fulladjr2
reg.summary.full$cp
reg.summary.full$bic
reg.summary.full$outmat
reg.summary.full$obj

# Plots for FWD Subset
par(mfrow=c(2,2)) 
  # Full model results in lowest RSS, duh.
  which.min(reg.summary.fwd$rss) # Minimize RSS
  plot(reg.summary.fwd$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
  points(23, reg.summary.fwd[23], col="red", cex=2, pch=20)

  # 19 predictor model results in lowest Adjusted Rsq.
  which.max(reg.summary.fwd$adjr2) # Maximum point of the vector
  plot(reg.summary.fwd$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") 
  points(19,reg.summary.fwd$adjr2[19], col="red",cex=2,pch=20) # Plot the point on the graph

  # 17 predictor model results in lowest Cp. Getting better
  which.min(reg.summary.fwd$cp)
  plot(reg.summary.fwd$cp ,xlab="Number of Variables ",ylab="Cp", type="l") 
  points(17,reg.summary.fwd$cp [17],col="red",cex=2,pch=20) 

  # 13 predictor model results in lowest BIC.
  which.min(reg.summary.fwd$bic )
  plot(reg.summary.fwd$bic ,xlab="Number of Variables ",ylab="bic", type="l")
  points(13,reg.summary.full$bic [13],col="red",cex=2,pch=20) 


  par(mfrow=c(1,1))
  plot(regfit.fwd,scale="r2")
  plot(regfit.fwd,scale="adjr2") > plot(regfit.full,scale="Adjusted Rsq") 
  plot(regfit.fwd,scale="Cp") > plot(regfit.full,scale="Cp") 
  plot(regfit.fwd,scale="bic") 

  # Plots for FULL Subset
  par(mfrow=c(2,2)) 
  # Full model results in lowest RSS, duh.
  which.min(reg.summary.full$rss) # Minimize RSS
  plot(reg.summary.full$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
  points(23, reg.summary.full$rss[23], col="red", cex=2, pch=20)
  
  # 19 predictor model results in lowest Adjusted Rsq.
  which.max(reg.summary.full$adjr2) # Maximum point of the vector
  plot(reg.summary.full$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") 
  points(19,reg.summary.full$adjr2[19], col="red",cex=2,pch=20) # Plot the point on the graph
  
  # 16 predictor model results in lowest Cp. Getting better
  which.min(reg.summary.full$cp)
  plot(reg.summary.full$cp ,xlab="Number of Variables ",ylab="Cp", type="l") 
  points(16,reg.summary.full$cp [16],col="red",cex=2,pch=20) 
  
  # 10 predictor model results in lowest BIC.
  which.min(reg.summary.full$bic )
  plot(reg.summary.full$bic ,xlab="Number of Variables ",ylab="bic", type="l")
  points(10,reg.summary.full$bic [10],col="red",cex=2,pch=20) 
  
  
  par(mfrow=c(1,1))
  plot(regfit.full,scale="r2")
  plot(regfit.full,scale="adjr2") > plot(regfit.full,scale="Adjusted Rsq") 
  plot(regfit.full,scale="Cp") > plot(regfit.full,scale="Cp") 
  plot(regfit.full,scale="bic")   
  

# review the 10 variable BIC model
coef(regfit.full, 10)

# Forward and backward stepwise Selection

