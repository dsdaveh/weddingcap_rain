# clustering for EDA purposes
require(lattice)
require(randomForest)
require(rpart)
require(rattle)

#load and prep the data if this hasn't been done
source("../team/data-prep.R")

# buckets
# - 0 - 70
# - 70 - 1000
# - 1000 - 1500
# - 1500 - 2300
# - 2300 - 3300
# - 3300 - 5000
# - 5000 +

train.sample1000$bin.expected <- ifelse(train.sample1000$Expected <= 70, 1, 
                             ifelse(train.sample1000$Expected > 70 & train.sample1000$Expected <=1000, 2,
                                    ifelse(train.sample1000$Expected > 1000 & train.sample1000$Expected <=1500, 3,
                                          ifelse(train.sample1000$Expected > 1500 & train.sample1000$Expected <=2300, 4,
                                                ifelse(train.sample1000$Expected > 2300 & train.sample1000$Expected <=3300, 5,
                                                      ifelse(train.sample1000$Expected > 3300 & train.sample1000$Expected <=5000, 6,
                                                            ifelse(train.sample1000$Expected > 5000, 7, 0)))))))
train.sample1000$bin.expected <- as.factor(train.sample1000$bin.expected)

# quality check - should be no rows in 0 bucket
sum(train$bin.expected == 0)

# scatterplots by cluster - time consuming
#xyplot(Ref ~ RhoHV | bin.expected, train, pch= 20)
#xyplot(Zdr ~ Kdp | bin.expected, train, pch= 20)

#######
#  models
#######

# exclude minute past and distance
frmla = bin.expected ~ Ref + Ref_5x5_10th + Ref_5x5_50th + Ref_5x5_90th + RefComposite + RefComposite_5x5_10th + RefComposite_5x5_50th + RefComposite_5x5_90th + RhoHV + RhoHV_5x5_10th + RhoHV_5x5_50th + RhoHV_5x5_90th + Zdr + Zdr_5x5_10th + Zdr_5x5_50th + Zdr_5x5_90th + Kdp + Kdp_5x5_10th + Kdp_5x5_50th + Kdp_5x5_90th

# random forest to check if any vars can help separate
# need to do a lot of imputing for this to work
set.seed(498)
model.rf <- randomForest(formula=frmla, data=train.sample1000, na.action=na.roughfix)
print(model.rf)
summary(model.rf)
round(importance(model.rf), 2)
plot(model.rf, log='y')
par(mfrow=c(1,1))
varImpPlot(model.rf)


#tree - predicts everything as a 1
model.tree <- rpart(formula=frmla, data=train, method="class")
par(mfrow=c(1,1), mai=c(1.5,1.2,0.8,1.0))
fancyRpartPlot(model.tree)
tree.pred.train <- predict(model.tree, train, type="class")
tree.confusion.train <- table(observed=train$bin.expected, predicted=tree.pred.train)
tree.confusion.train
sum(diag(tree.confusion.train)) / sum(tree.confusion.train)
