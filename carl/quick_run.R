setwd("/Users/ckoster/RStudio Projects/weddingcap_rain/carl")
train <- fread("../train.csv", stringsAsFactors = FALSE) 
train <- subset(train, Expected <= 12000)
#train <- subset(train, Expected <= 60 & Expected > 55)
#train <- subset(train, Expected > 110 & Expected <= 115)
#train <- subset(train, Expected > 5 & Expected <= 10)
#train <- data.table(train)
#train = read.table("../train.csv")
save(train, file="train_full.RData")
rdata_file <- "train_full.RData"
source("../carl/KS_gbm_cv_org.R")
nrow(train)
min(train$Expected)
max(train$Expected)


MAE_study_1 <- fread("MAE_study_1.csv")
MAE_study_1 <- subset(MAE_study_1, Expected > 2600 & Expected < 30000)
plot(MAE_study_1$Expected,MAE_study_1$MAE_CV_TRAIN)
