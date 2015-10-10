## For MAE, median is the best single estimate you can make
## Here we just get the median of the entire training set and 
##   use that single number as a predictor.
## We also show the MAE of using that number against the training set, using
##   two methods, showing data.table's fast aggregations.
## It is drawn from the full distribution, so it isn't the best judge of
##   generalization. 
## Also demonstrates using data.table to read a file quickly and only 
##   the columns you need (just Id and the target, in this case).
source( "../team/data-prep.R")

library(data.table)
library(Metrics)
## use data table to only read the Estimated, Ref, and Id fields
print(paste("reading training file:",Sys.time()))
#train<-fread("../input/train.csv",select=c(1,4,24))

print(paste("calculate median:",Sys.time()))
## obtain the median
##  * count the records and the NAs, so we can remove them
##  * we are taking the mean of the target, but any aggregation will work, since
##    the target values are all identical for each ID
##  * after the records have been grouped and invalids discarded, calculate the median
idAggregations<-train[,.(
    target = mean(Expected),
    records = .N,
    naCounts = sum(is.na(Ref))
),Id][records>naCounts,]
singleEstimateById<-idAggregations[,median(target)]
print(paste("median by ID: ",singleEstimateById))
## see what the MAE on the training set would be with this
mae(idAggregations[,target],singleEstimateById)  

print(paste("reading submission file:",Sys.time()))
## create a submission with this estimate
submission<-fread("../input/sample_solution.csv")
submission$Expected<-singleEstimateById
print(paste("writing predictions:",Sys.time()))
write.csv(submission,"median_benchmark.csv",row.names=F)
print(paste("all done:",Sys.time()))