# Linear Model Selection

library(leaps)

# Best subset selection
# BIC suggests the smalletst model with 10 predictors: 

# BIC with train.complete.cases: 
# minutes_past + radardist_km + Ref_5x5_50th + Ref_5x5_90th + RefComposite_5x5_10th + RefComposite_5x5_50th + RhoHV_5x5_10th + RhoHV_5x5_50th + RhoHV_5x5_90th + Zdr_5x5_50th 

# BIC with complete training set:
# 

# Initial results seem to be the same (in terms of num of predictors) regardless of full train set or a subset (train.complete.cases)

#regfit.full=regsubsets(Expected~.,data=train.complete.cases, nvmax=23)
regfit.full=regsubsets(Expected~., data=train, nvmax=23)
reg.summary <- summary(regfit.full)

reg.summary$which
reg.summary$rsq
reg.summary$rss
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
reg.summary$outmat
reg.summary$obj

par(mfrow=c(2,2)) 

  # Full model results in lowest RSS, duh.
  which.min(reg.summary$rss) # Minimize RSS
  plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
  points(23, reg.summary$rss[23], col="red", cex=2, pch=20)

  # 19 predictor model results in lowest Adjusted Rsq.
  which.max(reg.summary$adjr2) # Maximum point of the vector
  plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") 
  points(19,reg.summary$adjr2[19], col="red",cex=2,pch=20) # Plot the point on the graph

  # 16 predictor model results in lowest Cp. Getting better
  which.min(reg.summary$cp)
  plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l") 
  points(16,reg.summary$cp [16],col="red",cex=2,pch=20) 

  # 10 predictor model results in lowest BIC.
  which.min(reg.summary$bic )
  plot(reg.summary$bic ,xlab="Number of Variables ",ylab="bic", type="l")
  points(10,reg.summary$bic [10],col="red",cex=2,pch=20) 


par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2") > plot(regfit.full,scale="Adjusted Rsq") 
plot(regfit.full,scale="Cp") > plot(regfit.full,scale="Cp") 
plot(regfit.full,scale="bic") 

# review the 10 variable BIC model
coef(regfit.full, 10)
