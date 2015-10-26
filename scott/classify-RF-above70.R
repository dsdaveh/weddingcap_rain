#####
#
#  Random Forest to classify above and below a cut point
# 
#####

library(dplyr)
library(data.table)
library(randomForest)
library(rpart)
library(rattle)

source("../team/rain_utils.R")
source("../team/data-prep.R")

# collapse rows using means across
forest.dt <- train[ , .(
  radardist_km = max(radardist_km, na.rm=TRUE)
  ,records = .N
  ,ref.mean = weighted.mean(Ref, duration, na.rm=TRUE)
  ,ref.10thmean = weighted.mean(Ref_5x5_10th, duration, na.rm=TRUE)
  ,ref.50thmean = weighted.mean(Ref_5x5_50th, duration, na.rm=TRUE)
  ,ref.90thmean = weighted.mean(Ref_5x5_90th, duration, na.rm=TRUE)
  ,refcomp.mean = weighted.mean(RefComposite, duration, na.rm=TRUE)
  ,refcomp.10thmean = weighted.mean(RefComposite_5x5_10th, duration, na.rm=TRUE)
  ,refcomp.50thmean = weighted.mean(RefComposite_5x5_50th, duration, na.rm=TRUE)
  ,refcomp.90thmean = weighted.mean(RefComposite_5x5_90th, duration, na.rm=TRUE)
  ,rho.mean = weighted.mean(RhoHV, duration, na.rm=TRUE)
  ,rho.10thmean = weighted.mean(RhoHV_5x5_10th, duration, na.rm=TRUE)
  ,rho.50thmean = weighted.mean(RhoHV_5x5_50th, duration, na.rm=TRUE)
  ,rho.90thmean = weighted.mean(RhoHV_5x5_90th, duration, na.rm=TRUE)
  ,zdr.mean = weighted.mean(Zdr, duration, na.rm=TRUE)
  ,zdr.10thmean = weighted.mean(Zdr_5x5_10th, duration, na.rm=TRUE)
  ,zdr.50thmean = weighted.mean(Zdr_5x5_50th, duration, na.rm=TRUE)
  ,zdr.90thmean = weighted.mean(Zdr_5x5_90th, duration, na.rm=TRUE)
  ,kdp.mean = weighted.mean(Kdp, duration, na.rm=TRUE)
  ,kdp.10thmean = weighted.mean(Kdp_5x5_10th, duration, na.rm=TRUE)
  ,kdp.50thmean = weighted.mean(Kdp_5x5_50th, duration, na.rm=TRUE)
  ,kdp.90thmean = weighted.mean(Kdp_5x5_90th, duration, na.rm=TRUE)
  ,Measured = max(Expected)
  ,m.palmer=mpalmer(Ref, minutes_past)
), by=Id] 


# set the classes
forest.dt$above70 <- ifelse(forest.dt$Measured > 70, 1, 0) # 28,370
forest.dt$above40 <- ifelse(forest.dt$Measured > 40, 1, 0) # 33,711
forest.dt$above70 <- as.factor(forest.dt$above70)
forest.dt$above40 <- as.factor(forest.dt$above40)

#save if interested in returning to this point
save(forest.dt, file="../forest-dt.RData")
#load("../forest-dt.RData")

# get a random sample of 28,730 Ids below 70 for a 50-50 split to train
set.seed(498)
Ids.samplebelow70 <- sample(forest.dt[forest.dt$above70==0,]$Id, 28370)
treetrain <- rbind(forest.dt[Ids.samplebelow70,], forest.dt[forest.dt$above70==1,])

# formula
n <- names(forest.dt)
form70 <- as.formula(paste("above70 ~", paste(n[!n %in% c("above70", "above40", "Id", "Measured")], collapse = " + ")))
form40 <- as.formula(paste("above40 ~", paste(n[!n %in% c("above70", "above40", "Id", "Measured")], collapse = " + ")))

###  
# works 53% and lots of false positives, bad for MAE 
# train a basic tree to predict above 40
treemodel.all <- rpart(formula=form40, data=treetrain, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.pred.train <- predict(treemodel.all, newdata=forest.dt, type="class")
treemodel.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.pred.train )
treemodel.confusion.train
sum(diag(treemodel.confusion.train)) / sum(treemodel.confusion.train) # 0.5372025

# try just refs
treemodel.refs <- rpart(formula=above40~ref.mean+ref.10thmean+ref.50thmean+ref.90thmean
                        +refcomp.mean+refcomp.10thmean+refcomp.50thmean+refcomp.90thmean
                        +radardist_km+records,
                        data=forest.dt, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.refs.pred.train <- predict(treemodel.refs, treetrain, type="class")
treemodel.refs.confusion.train <- table(observed=treetrain$above40, predicted=treemodel.refs.pred.train )
treemodel.refs.confusion.train

###
# random forest works 92.7% when it assigns a value, but predicts 401693 NAs!!
# need to come back to this after imputing
set.seed(498)
system.time(rfmodel.allorig <- randomForest(form40, data=treetrain, na.action=na.roughfix, importance=TRUE, ntree=200))

rfmodel.allorig.pred <- predict(rfmodel.allorig, newdata=forest.dt, type="response")
rfmodel.allorig.confusion <- table(observed=forest.dt$above40, predicted=rfmodel.allorig.pred)
rfmodel.allorig.confusion
sum(diag(rfmodel.allorig.confusion)) / sum(rfmodel.allorig.confusion) # 0.9273136

print(rfmodel.allorig)
summary(rfmodel.allorig)
round(importance(rfmodel.allorig), 2)
plot(rfmodel.allorig, log='y')
par(mfrow=c(1,1))
varImp(rfmodel.allorig)
varImpPlot(rfmodel.allorig)

###
# logistic regression
# same (?) 401693 NAs
logit <- glm(form40, data = treetrain, family = "binomial")
logit.pred <- predict(logit, newdata = forest.dt, type = "response")
logit.pred.class <- round(logit.pred)
logit.confusion <- table(observed=forest.dt$above40, predicted=logit.pred.class)
logit.confusion
sum(diag(logit.confusion)) / sum(logit.confusion) # 0.7694106


