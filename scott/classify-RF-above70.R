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
  ,ref.mean = mean(Ref, na.rm=TRUE)
  ,ref.10thmean = mean(Ref_5x5_10th, na.rm=TRUE)
  ,ref.50thmean = mean(Ref_5x5_50th, na.rm=TRUE)
  ,ref.90thmean = mean(Ref_5x5_90th, na.rm=TRUE)
  ,refcomp.mean = mean(RefComposite, na.rm=TRUE)
  ,refcomp.10thmean = mean(RefComposite_5x5_10th, na.rm=TRUE)
  ,refcomp.50thmean = mean(RefComposite_5x5_50th, na.rm=TRUE)
  ,refcomp.90thmean = mean(RefComposite_5x5_90th, na.rm=TRUE)
  ,rho.mean = mean(RhoHV, na.rm=TRUE)
  ,rho.10thmean = mean(RhoHV_5x5_10th, na.rm=TRUE)
  ,rho.50thmean = mean(RhoHV_5x5_50th, na.rm=TRUE)
  ,rho.90thmean = mean(RhoHV_5x5_90th, na.rm=TRUE)
  ,zdr.mean = mean(Zdr, na.rm=TRUE)
  ,zdr.10thmean = mean(Zdr_5x5_10th, na.rm=TRUE)
  ,zdr.50thmean = mean(Zdr_5x5_50th, na.rm=TRUE)
  ,zdr.90thmean = mean(Zdr_5x5_90th, na.rm=TRUE)
  ,kdp.mean = mean(Kdp, na.rm=TRUE)
  ,kdp.10thmean = mean(Kdp_5x5_10th, na.rm=TRUE)
  ,kdp.50thmean = mean(Kdp_5x5_50th, na.rm=TRUE)
  ,kdp.90thmean = mean(Kdp_5x5_90th, na.rm=TRUE)
  ,Measured = max(Expected)
  ,m.palmer=mpalmer(Ref, minutes_past)
), by=Id] 

#  at some point need to fix the NA & weighted.mean problem; ideally by imputing columns vertically
# forest.dt <- train[ , .(
#   radardist_km = max(radardist_km, na.rm=TRUE)
#   ,records = .N
#   ,ref.mean = weighted.mean(Ref, duration, na.rm=TRUE)
#   ,ref.10thmean = weighted.mean(Ref_5x5_10th, duration, na.rm=TRUE)
#   ,ref.50thmean = weighted.mean(Ref_5x5_50th, duration, na.rm=TRUE)
#   ,ref.90thmean = weighted.mean(Ref_5x5_90th, duration, na.rm=TRUE)
#   ,refcomp.mean = weighted.mean(RefComposite, duration, na.rm=TRUE)
#   ,refcomp.10thmean = weighted.mean(RefComposite_5x5_10th, duration, na.rm=TRUE)
#   ,refcomp.50thmean = weighted.mean(RefComposite_5x5_50th, duration, na.rm=TRUE)
#   ,refcomp.90thmean = weighted.mean(RefComposite_5x5_90th, duration, na.rm=TRUE)
#   ,rho.mean = weighted.mean(RhoHV, duration, na.rm=TRUE)
#   ,rho.10thmean = weighted.mean(RhoHV_5x5_10th, duration, na.rm=TRUE)
#   ,rho.50thmean = weighted.mean(RhoHV_5x5_50th, duration, na.rm=TRUE)
#   ,rho.90thmean = weighted.mean(RhoHV_5x5_90th, duration, na.rm=TRUE)
#   ,zdr.mean = weighted.mean(Zdr, duration, na.rm=TRUE)
#   ,zdr.10thmean = weighted.mean(Zdr_5x5_10th, duration, na.rm=TRUE)
#   ,zdr.50thmean = weighted.mean(Zdr_5x5_50th, duration, na.rm=TRUE)
#   ,zdr.90thmean = weighted.mean(Zdr_5x5_90th, duration, na.rm=TRUE)
#   ,kdp.mean = weighted.mean(Kdp, duration, na.rm=TRUE)
#   ,kdp.10thmean = weighted.mean(Kdp_5x5_10th, duration, na.rm=TRUE)
#   ,kdp.50thmean = weighted.mean(Kdp_5x5_50th, duration, na.rm=TRUE)
#   ,kdp.90thmean = weighted.mean(Kdp_5x5_90th, duration, na.rm=TRUE)
#   ,Measured = max(Expected)
#   ,m.palmer=mpalmer(Ref, minutes_past)
# ), by=Id] 


#re-impute Ref
unmp <- function (r) log( (r ^ 1.6) * 200, 10) * 10  #back out Ref from M-P prediction
forest.dt$ref.mean <- ifelse(is.nan(forest.dt$ref.mean), unmp(forest.dt$m.palmer), forest.dt$ref.mean)

# set the classes
forest.dt$above1000 <- ifelse(forest.dt$Measured > 1000, 1, 0) # 9804
forest.dt$above70 <- ifelse(forest.dt$Measured > 70, 1, 0) # 28,370
forest.dt$above40 <- ifelse(forest.dt$Measured > 40, 1, 0) # 33,711
forest.dt$above1000 <- as.factor(forest.dt$above1000)
forest.dt$above70 <- as.factor(forest.dt$above70)
forest.dt$above40 <- as.factor(forest.dt$above40)

# these are the target numbers of rows to predict - above the threshold value and Ref has a value
# above1000 : 4864
# above70 :   17662
# above40 :   21999
forest.dt %>% filter(above1000==1) %>% nrow() # 9804
forest.dt %>% filter(above1000==1) %>% filter(is.nan(ref.mean)) %>% nrow() # 4940
forest.dt %>% filter(above70==1) %>% nrow() # 28370
forest.dt %>% filter(above70==1) %>% filter(is.nan(ref.mean)) %>% nrow() # 10708
forest.dt %>% filter(above40==1) %>% nrow() # 33711
forest.dt %>% filter(above40==1) %>% filter(is.nan(ref.mean)) %>% nrow() # 11712


#this only works if you run kdp_all_means.R to generate predictions
forest.dt <- cbind(forest.dt, pred.alluseabove70$predict)

#save if interested in returning to this point
save(forest.dt, file="../forest-dt.RData")
#load("../forest-dt.RData")

# get a random sample of 28,730 Ids below 70 for a 50-50 split to train
set.seed(498)
Ids.samplebelow70 <- sample(forest.dt[forest.dt$above70==0,]$Id, 28370)
Ids.samplebelow10 <- sample(forest.dt[forest.dt$Measured<10,]$Id, 28370)
treetrain <- rbind(forest.dt[Id %in% Ids.samplebelow10,], forest.dt[forest.dt$above70==1,])
treetrain$V2.norm <- scale(treetrain$V2, center=TRUE, scale=TRUE)

# formula
n <- names(forest.dt)
form1000 <- as.formula(paste("above1000 ~", paste(n[!n %in% c("above70", "above40", "above1000", "Id", "Measured")], collapse = " + ")))
form1000.90th <- as.formula("above1000~radardist_km+ref.90thmean+refcomp.90thmean+rho.90thmean+zdr.90thmean+kdp.90thmean+V2")
form70 <- as.formula(paste("above70 ~", paste(n[!n %in% c("above70", "above40", "above1000", "Id", "Measured")], collapse = " + ")))
form40 <- as.formula(paste("above40 ~", paste(n[!n %in% c("above70", "above40", "above1000", "Id", "Measured")], collapse = " + ")))
form40.90th <- as.formula("above40~radardist_km+ref.90thmean+refcomp.90thmean+rho.90thmean+zdr.90thmean+kdp.90thmean+V2")


###  
# works 86.8% and lots of false positives, bad for MAE 
# train a basic tree to predict above 40
treemodel.all <- rpart(formula=form70, data=treetrain, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.pred.train <- predict(treemodel.all, newdata=forest.dt, type="class")
treemodel.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.pred.train )
treemodel.confusion.train
sum(diag(treemodel.confusion.train)) / sum(treemodel.confusion.train) # 0.7607541 ; 18k correct above 70

treemodel.basic <- rpart(formula=above70~ref.mean+m.palmer+V2, data=treetrain, method="class")
treemodel.basic.pred.train <- predict(treemodel.basic, newdata=forest.dt, type="class")
treemodel.basic.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.basic.pred.train )
treemodel.basic.confusion.train
sum(diag(treemodel.basic.confusion.train)) / sum(treemodel.basic.confusion.train) # 0.7573277 ; 17k correct above 70


# try just refs
treemodel.refs <- rpart(formula=above40~ref.mean+ref.10thmean+ref.50thmean+ref.90thmean
                        +refcomp.mean+refcomp.10thmean+refcomp.50thmean+refcomp.90thmean
                        +radardist_km+records,
                        data=treetrain, method="class")
#fancyRpartPlot(treemodel.all)
#treemodel.all
treemodel.refs.pred.train <- predict(treemodel.refs, newdata=forest.dt, type="class")
treemodel.refs.confusion.train <- table(observed=forest.dt$above40, predicted=treemodel.refs.pred.train )
treemodel.refs.confusion.train

###
# random forest works 92.7% when it assigns a value, but predicts 401693 NAs!!
# need to come back to this after imputing
set.seed(498)
system.time(rfmodel.allorig <- randomForest(form70, data=treetrain, na.action=na.roughfix, importance=TRUE, ntree=200))

rfmodel.allorig.pred <- predict(rfmodel.allorig, newdata=forest.dt, type="response")
rfmodel.allorig.confusion <- table(observed=forest.dt$above40, predicted=rfmodel.allorig.pred)
rfmodel.allorig.confusion
sum(diag(rfmodel.allorig.confusion)) / sum(rfmodel.allorig.confusion) # 0.979504

print(rfmodel.allorig)
summary(rfmodel.allorig)
round(importance(rfmodel.allorig), 2)
plot(rfmodel.allorig, log='y')
par(mfrow=c(1,1))
varImp(rfmodel.allorig)
varImpPlot(rfmodel.allorig)

###
# logistic regression
# same (?) 399387 NAs above 70; low number of correct predictions above 70
logit <- glm(form70, data = treetrain, family = binomial(link="logit"))
logit.pred <- predict(logit, newdata = forest.dt, type = "response")
logit.pred.class <- round(logit.pred)
logit.confusion <- table(observed=forest.dt$above70, predicted=logit.pred.class)
logit.confusion
sum(diag(logit.confusion)) / sum(logit.confusion) # 0.957088

logit.basic <- glm(above70~ref.mean+m.palmer+V2, data=treetrain, family=binomial(link="logit"))
logit.basic.pred <- predict(logit, newdata = forest.dt, type = "response")
logit.basic.pred.class <- round(logit.basic.pred)
logit.basic.confusion <- table(observed=forest.dt$above70, predicted=logit.basic.pred.class)
logit.basic.confusion
sum(diag(logit.basic.confusion)) / sum(logit.basic.confusion) # 0.9145233 ; only 2740 above 70

logit.1000 <- glm(form1000.90th, data = treetrain, family = binomial(link="logit"))
logit.1000.pred <- predict(logit.1000, newdata = forest.dt, type = "response")
logit.1000.pred.class <- round(logit.1000.pred)
logit.1000.confusion <- table(observed=forest.dt$above1000, predicted=logit.1000.pred.class)
logit.1000.confusion
sum(diag(logit.1000.confusion)) / sum(logit.1000.confusion) # 0.9840537

logit.40 <- glm(form40.90th, data = treetrain, family = binomial(link="logit"))
logit.40.pred <- predict(logit.40, newdata = forest.dt, type = "response")
logit.40.pred.class <- round(logit.40.pred)
logit.40.confusion <- table(observed=forest.dt$above40, predicted=logit.40.pred.class)
logit.40.confusion
sum(diag(logit.40.confusion)) / sum(logit.40.confusion) # 0.8878572 ; 253605 NAs
