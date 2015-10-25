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

# collapse rows using means across; might consider adding MP
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
), by=Id] 


# set the classes
forest.dt$above70 <- ifelse(forest.dt$Measured > 70, 1, 0)
forest.dt$above40 <- ifelse(forest.dt$Measured > 40, 1, 0)

#save if interested in returning to this point
save(forest.dt, file="../forest-dt.RData")
#load("../forest-dt.RData")

# formula
n <- names(forest.dt)
form70 <- as.formula(paste("above70 ~", paste(n[!n %in% c("above70", "above40", "Id", "Measured")], collapse = " + ")))
form40 <- as.formula(paste("above40 ~", paste(n[!n %in% c("above70", "above40", "Id", "Measured")], collapse = " + ")))

# train a basic tree with all vars - can't predict the above70s
treemodel.all <- rpart(formula=form40, data=forest.dt, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.pred.train <- predict(treemodel.all, forest.dt, type="class")
treemodel.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.pred.train )
treemodel.confusion.train

# try just refs
treemodel.refs <- rpart(formula=above40~ref.mean+ref.10thmean+ref.50thmean+ref.90thmean+refcomp.mean+refcomp.10thmean+refcomp.50thmean+refcomp.90thmean,
                        data=forest.dt, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.refs.pred.train <- predict(treemodel.refs, forest.dt, type="class")
treemodel.refs.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.refs.pred.train )
treemodel.refs.confusion.train


# didn't take time to let this finish - has trouble with fixing the NAs?
# set.seed(498)
# system.time(rfmodel.allorig <- randomForest(form, data=forest.dt, na.action=na.roughfix, ntree=20))
# print(rfmodel.allorig)
# summary(rfmodel.allorig)
# round(importance(rfmodel.allorig), 2)
# plot(rfmodel.allorig, log='y')
# par(mfrow=c(1,1))
# varImp(rfmodel.allorig)
# varImpPlot(rfmodel.allorig)

