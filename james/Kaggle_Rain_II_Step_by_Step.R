library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)
library(mice)

###List out plan###
#1) Import training dataset
#2) Remove Noisy Data - NA Ref values, outliers?
#3) Impute missing values - a) vertically
#4) Impute missing values - b) horizontally
#5) Collapse data by id apply unique summarise functions
#6) Divide data into likely errors via Expected & building classification model to predict errors
#7) Divide collapsed data into training and validation sets
#8) Model both training datasets with several models
#9) Select best model for each - lowest MAE on validation set
#10) Retrain models with entire training dataset
#11) import testing data and collapse vis-a-vis training data
#12) generate predictions for Expected columns
#13) uncollapse data to have list of final predictions

###1--Import Training dataset###
setwd('/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james')
source("../team/rain_utils.R")
source("../team/data-prep.R")
summary(train)
summary(train.sample1000)

#creating sample dataset slighlty bigger than 1000

  set.seed(491)
  Ids.sample5000 <- sample(unique(train$Id), 5000)
  train.sample5000 <- train[Id%in%Ids.sample5000,]


###2 Remove all null Ref values###
train_noNARefs <- filter(train.sample5000, (Ref != "NA"))



###3--Impute missing values##
#a--vertically##




###4--Impute missing values##
#b--horizontally##

#EDA - Understanding preliminary relationships
train_noNARefs_4_Fields <- select(train_noNARefs, Ref, Kdp, Zdr, RhoHV)
plot(train_noNARefs_4_Fields)

train_noNARefs_Ref_Fields <- select(train_noNARefs, Ref, Ref_5x5_10th, Ref_5x5_50th,Ref_5x5_90th, RefComposite)
plot(train_noNARefs_Ref_Fields)

train_noNARefs_Kdp_Fields <- select(train_noNARefs, Kdp, Kdp_5x5_10th, Kdp_5x5_50th,Kdp_5x5_90th)
plot(train_noNARefs_Kdp_Fields)

train_noNARefs_Zdr_Fields <- select(train_noNARefs, Zdr, Zdr_5x5_10th, Zdr_5x5_50th,Kdp_5x5_90th)
plot(train_noNARefs_Zdr_Fields)

train_noNARefs_RhoHV_Fields <- select(train_noNARefs, RhoHV, RhoHV_5x5_10th, RhoHV_5x5_50th,RhoHV_5x5_90th)
plot(train_noNARefs_RhoHV_Fields)


#EDA - Basic Imputing Graphs - not actually used, just fun to see whats going on with imputation method. Only 4 variables plotted here, 24 variables are too much to handle.
imp_trainNoNARefs <- mice(train_noNARefs_4_Fields, print = FALSE, m =5, seed = 2)
fit <- with( imp_trainNoNARefs, lm(Ref ~ Kdp + Zdr + RhoHV))
est<- pool(fit)
attributes(est)
est$qhat
summary(est)
est$qbar
est$lambda
imp_trainNoNARefs
head(train_noNARefs_4_Fields)
train_noNARefs_4_Fields[0:20]
md.pattern(train_noNARefs_4_Fields)
imp_trainNoNARefs <- mice(data = train_noNARefs_4_Fields, print = FALSE, maxit =5, seed = 2)
plot(imp_trainNoNARefs)
stripplot(imp_trainNoNARefs, pch =20, cex = 1.2)

#Actual Imputing of Dataset using Predictive Mean Matching
train_noNARefs_noIDField <- select(train_noNARefs, -Id)
imp_trainNoNARefs <- mice(data = train_noNARefs_noIDField, print = FALSE, maxit =2, seed = 2)
train_noNARefs_imputed <- complete(imp_trainNoNARefs, action = 3)
train_noNARefs_imputed$Id <- train_noNARefs$Id


###5) Collapse data by id apply unique summarise functions###

ID_grouped <- train_noNARefs_imputed%>%
  select(Id, radardist_km, minutes_past,Ref, RefComposite, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(radardist_km = median(radardist_km),
            Min_Ref = min(Ref, na.rm = TRUE), 
            Max_Ref = max(Ref, na.rm = TRUE),
            Mean_Ref = mean(Ref, na.rm = TRUE),
            mpalmer = mpalmer(Ref, minutes_past),
            Min_RhoHV = min(RhoHV, na.rm = TRUE),
            Max_RhoHV = max(RhoHV, na.rm = TRUE),
            Mean_RhoHV = mean(RhoHV, na.rm=TRUE),
            Min_Zdr = min(Zdr, na.rm = TRUE),
            Max_Zdr = max(Zdr, na.rm = TRUE),
            Mean_Zdr = mean(Zdr, na.rm = TRUE),
            Min_Kdp = min(Kdp, na.rm = TRUE),
            Max_Kdp = max(Kdp, na.rm = TRUE),
            Mean_Kdp = mean(Kdp, na.rm = TRUE),
            Expected = median(Expected)) %>%
  mutate(Ref_spread = Max_Ref - Min_Ref, 
         RhoHV_spread = Max_RhoHV - Min_RhoHV,
         Zdr_spread = Max_Zdr - Min_Zdr,
         Kdp_spread = Max_Kdp - Min_Kdp) 



####6) Divide data into likely errors via Expected & building classification model to predict errors###
#A - Setting Error Classification boundary###
ID_grouped$greaterthan55flag <- ifelse(ID_grouped$Expected >=55, 1,0)
ID_grouped_lessthan55 <- filter(ID_grouped, (greaterthan55flag == 0))
ID_grouped_greaterthan55 <- filter(ID_grouped, (greaterthan55flag == 1))

#B - Building Classification model to predict errors
set.seed(6)
random_sample <- sample(878, 700)
ID_grouped_train <- ID_grouped[random_sample,]
ID_grouped_val <- ID_grouped[-random_sample,]
summary(ID_grouped)
library(tree)

tree.ExpectedErrors = tree(greaterthan55flag ~. -Id -Expected, data = ID_grouped_train)
tree.ExpectedErrors
summary(tree.ExpectedErrors)
plot(tree.ExpectedErrors)
text(tree.ExpectedErrors, pretty=0)

tree.pred = predict(tree.ExpectedErrors, ID_grouped_val)
table(tree.pred, ID_grouped_val$greaterthan55flag)

forest_model <- predict(forest.ExpectedErrors, data = ID_grouped[-random_sample,], type = "class")
test = forest_model
table(forest_model,ID_grouped[-random_sample,])

length(forest_model)
length(ID_grouped[-random_sample,])
test = ID_grouped[random_sample,]

#Mean_Kdp <-7.03(right), Zdr spread <.28 (right)
#When ZDR spread is greater than .28 than errors become more likely

#Mean_Kdp < -4.23595 (R), Mean_RhoHV < .96 (R), Mean_Zdr < -0.426136(r), Max_Ref <59.5(l), Mean_Ref<.1 (right-false) 

#Max_Zdr < -0.59(R), Max_Ref<2.75(L) - 1/2, Max_Ref<2.75(R), Min_Ref<31.5, Mean_Kdp <1.85812 (l) 12/179.

#Max_Zdr <.21875(R), Ref_spread <39.25(R), Ref_spread<39.75(R) 1/8, radardist_km <7.5(r), 3/154

#Max_zdr < .22 (r), Mean_RhoHV <1.03 (l), MeanRhoHV <.78 (r)
library(randomForest)
set.seed(5)
random_sample <- sample(878, 700)


forest.ExpectedErrors = randomForest(greaterthan55flag ~ ., type = "class", data = ID_grouped[random_sample,],
                        mtry =1, importance = TRUE)
forest.ExpectedErrors
forest_model <- predict(forest.ExpectedErrors, data = ID_grouped[-random_sample,], type = "class")
test = forest_model
table(forest_model,ID_grouped[-random_sample,])

length(forest_model)
length(ID_grouped[-random_sample,])
test = ID_grouped[random_sample,]
if()



#7) Divide collapsed data into training and validation sets

#EDA - Understanding preliminary relationships
ID_grouped_EDA_Ref <- select(ID_grouped, Expected, mpalmer, Mean_Ref, Ref_spread)
plot(ID_grouped_EDA_Ref)
ID_grouped_EDA_RhoHV <- select(ID_grouped, Expected, Mean_RhoHV, RhoHV_spread)
plot(ID_grouped_EDA_RhoHV)
ID_grouped_EDA_Kdp <- select(ID_grouped, Expected, Mean_Kdp, Kdp_spread)
plot(ID_grouped_EDA_Kdp)
ID_grouped_EDA_Zdr <- select(ID_grouped, Expected, Mean_Zdr, Zdr_spread)
plot(ID_grouped_EDA_Zdr)

#Build linear model predicting Expected
set.seed(2)
random_sample <- sample(875, 700)
lm.fit <- lm(Expected ~ mpalmer + Mean_Ref + Mean_RhoHV + Mean_Zdr + Mean_Kdp +
               Ref_spread + RhoHV_spread + Zdr_spread + Kdp_spread ,
             data = ID_grouped, subset = random_sample)
#MAE
predict(lm.fit, ID_grouped)[random_sample]
MAE <- mean(abs(ID_grouped$Expected - predict(lm.fit, ID_grouped))[-random_sample])
print(MAE)
summary(lm.fit_3)
#Build linear model predicting Ref only RhoHV exists
lm.fit_RhoHV <- lm(Ref ~ RhoHV, data = Train_wRef, subset = random_sample)
#MAE
MAE <- mean(abs(Train_wRef$Ref - predict(lm.fit_RhoHV, Train_wRef))[-random_sample])
print(MAE)
summary(lm.fit_RhoHV)







