# Linear Model Selection

library(leaps)

# Best subset selection

regfit.full=regsubsets(Expected~.,data=train.complete.cases, nvmax=23)
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
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l") 
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l") 
which.max(reg.summary$adjr2) # Maximum point of the vector
points(19,reg.summary$adjr2[19], col="red",cex=2,pch=20) # Plot the point on the graph
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="RSS", type="l") 
which.min(reg.summary$cp)
points(16,reg.summary$cp [16],col="red",cex=2,pch=20) 
which.min(reg.summary$bic )
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="bic", type="l")
points(10,reg.summary$bic [10],col="red",cex=2,pch=20) 

# This is a bit tought to read
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2") > plot(regfit.full,scale="Cp") # Lowest is 16 + intercept
plot(regfit.full,scale="bic") # lowest BIX is intercept + radardist_km

