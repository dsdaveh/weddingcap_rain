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
#6) Divide data into >70mm Expected and <70mm Expected to separate true entries from errors
#7) Divide collapsed data into training and validation sets
#8) Model both training datasets with several models
#9) Select best model for each - lowest MAE on validation set
#10) Retrain models with entire training dataset
#11) import testing data and collapse vis-a-vis training data
#12) generate predictions for Expected columns
#13) uncollapse data to have list of final predictions

#1--Import Training dataset#
setwd('/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james')
source("../team/rain_utils.R")
source("../team/data-prep.R")
summary(train)
summary(train.sample1000)

#2 Remove all null Ref values
train_noNARefs <- filter(train.sample1000, (Ref != "NA"))



##3--Impute missing values##
#a--vertically##




##4--Impute missing values##
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
imp_trainNoNARefs <- mice(data = train_noNARefs_noIDField, print = FALSE, maxit =5, seed = 2)
train_noNARefs_imputed <- complete(imp_trainNoNARefs, action = 3)
train_noNARefs_imputed$Id <- train_noNARefs$Id


#5) Collapse data by id apply unique summarise functions

ID_grouped <- train_noNARefs_imputed%>%
  select(Id, minutes_past,Ref, RefComposite, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(radardist_km = median(radardis_km),
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













