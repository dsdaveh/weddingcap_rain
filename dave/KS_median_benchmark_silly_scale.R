## if MAE is ~24 and median is 1.27,  a better median MUST be higher,
## since we can't have negative rainfall right?
submission<-fread("../input/sample_solution.csv")
submission$Expected<-17.0
write.csv(submission,"median_benchmark_silly_scale.csv",row.names=F)

## apparently not ... scored 35.32880
