setwd("./")
library(h2o)
localH2O <- h2o.init(nthread=-1,max_mem_size="4G")

train <- read.csv("train_enh.csv")

train.hex <- as.h2o(localH2O,train)
predictors <- c(which(names(train.hex) == "NumMosquitos"), which(names(train.hex) == "d_nr"),
                which(names(train.hex) == "Tmax_M"), which(names(train.hex) == "PrecipTotal_M"),
                which(names(train.hex) == "y_nr"),which(names(train.hex) == "WetBulb"))

response <- which(names(train.hex) == "WnvPresent")

test <- read.csv("test_enh.csv")

l1 <- (which(train$Species == "CULEX ERRATICUS") | which(train$Species == "CULEX SALINARIUS")) 
l2 <- (which(train$Species == "CULEX TARSALIS") |  which(train$Species == "CULEX TERRITANS"))

submission <- as.data.frame(test$Id)
submission$WnvPresent <- 0

test.hex <- as.h2o(localH2O,test[,2:(ncol(test))])
  
for(j in 1:2){
    print(j)
    
    model <- h2o.deeplearning(x=predictors,
                              y=response,
                              data=train.hex,
                              classification=T,
                              activation="RectifierWithDropout",
                              hidden=c(1024,512,256),
                              hidden_dropout_ratio=c(0.5,0.5,0.5),
                              input_dropout_ratio=0.05,
                              epochs=20,
                              l1=1e-5,
                              l2=1e-5,
                              rho=0.99,
                              epsilon=1e-8,
                              train_samples_per_iteration=500,
                              max_w2=10,
                              seed=1)
    submission$WnvPresent <- submission$WnvPresent + as.data.frame(h2o.predict(model,test.hex))$X1
    print(j)
}

submission$WnvPresent <- submission$WnvPresent/j
names(submission) <- c('Id','WnvPresent')
#submission$WnvPresent[l1] <- 0
#submission$WnvPresent[l2] <- 0

write.csv(submission,file="submission_h2o.csv",row.names=FALSE)     