# Linear Model Selection

library(leaps)

# Best subset selection

regfit.full=regsubsets(Expected~.,data=train.sample1000 nvmax=23)
summary(regfit.full)