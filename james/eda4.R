library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)
library(mice)

###List out plan###
#1) Import training dataset
#1.5) Remove NA Ref values
#2) Impute missing values - a) vertically
#3) Remove any unusable rows following vertical impute (i.e. those rows all blank horizontally)
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

#1.5 Remove all null Ref values
train_noNARefs <- filter(train.sample1000, (Ref != "NA"))



##2--Impute missing values##
#a--vertically##
#if prior and after are not null- impute with average. 
#if prior is not null but after is - impute with prior value
#if prior is null but after is not - impute with after value
#repeat until all NAs are filled

#Filtering out all IDs that just have all NA entries
  train.ref.noVertAllNAs <- train.sample1000 %>%
  group_by(Id) %>%
  mutate( allNA = all( is.na(Ref))) %>%
  filter(! allNA )


test <- train.ref.noVertAllNAs
#impute remaining values vertically by imputing surrounding values

for (i in 2:nrow(test)){

  if((is.na(test$Ref[i])== TRUE))
  {
   test$Ref[i]=50000000
  }
    else { 
    test$Ref[i] = test$Ref[i]
    }
    
    
    n<- 1
    while (is.na(test$Ref[i+n]))
    {
      n++
    }
    test$Ref[i] = test$Ref[i+n]
  } else if( (is.na(test$Ref[i])== TRUE) && 
               test$Id[i] = test$Id[i-1])
{
    test$Ref[i] = test$Ref[i-1]
} else {
  test$Ref[i] = test$Ref[i]
}
}
    
    
   








##3--Remove remaining all row NAs##
Train_noNAonlys <- filter(train, (Ref != "NA") | (RhoHV != "NA")
                     & (Zdr != "NA") | (Kdp != "NA"))

##4--Impute missing values##
#b--horizontally##

####Imputing NaN Ref values ####
#RhoHV, Zdr, and Kdp all exist, but no Ref
Train_wRef <- filter(Train_noNAonlys, (Ref != "NA") & (RhoHV != "NA")
                                        & (Zdr != "NA") & (Kdp != "NA"))

#Build linear model predicting Ref -RhoHV, Zdr, and Kdp all exist, but no Ref
set.seed(5)
random_sample <- sample(7214513, 4000000)
lm.fit_3 <- lm(Ref ~ RhoHV + Zdr + Kdp, data = Train_wRef, subset = random_sample)
#MAE
MAE <- mean(abs(Train_wRef$Ref - predict(lm.fit_3, Train_wRef))[-random_sample])
print(MAE)
summary(lm.fit_3)
#Build linear model predicting Ref only RhoHV exists
lm.fit_RhoHV <- lm(Ref ~ RhoHV, data = Train_wRef, subset = random_sample)
#MAE
MAE <- mean(abs(Train_wRef$Ref - predict(lm.fit_RhoHV, Train_wRef))[-random_sample])
print(MAE)
summary(lm.fit_RhoHV)

#Build linear model predicting Ref only Zdr exists
lm.fit_Zdr <- lm(Ref ~ Zdr, data = Train_wRef, subset = random_sample)
#MAE
MAE <- mean(abs(Train_wRef$Ref - predict(lm.fit_Zdr, Train_wRef))[-random_sample])
print(MAE)
summary(lm.fit_Zdr)

#Build linear model predicting Ref only Kdp exists
lm.fit_Kdp <- lm(Ref ~ Kdp, data = Train_wRef, subset = random_sample)
#MAE
MAE <- mean(abs(Train_wRef$Ref - predict(lm.fit_Kdp, Train_wRef))[-random_sample])
print(MAE)
summary(lm.fit_Kdp)



#Build Random Forest predicting Ref - RhoHV, Zdr, and Kdp all exist, but no Ref
library(randomForest)
set.seed(5)
forest.ref_impute = randomForest(Ref ~ RhoHV + Zdr + Kdp,data = Train_wRef, subset = random_sample, 
                                 mtry =1, importance = TRUE)
forest.ref_impute
yhat.forest <- predict(forest.ref_impute, newdata = Train_wRef[-random_sample])
plot(yhat.forest, Train_wRef[-random_sample]$Ref)
abline(0,1)
#MAE
mean(abs((yhat.forest - Train_wRef[-random_sample]$Ref)))

#Imputing ref values - To Be continued
Train_noRef <-filter(Train_noNAonlys, (Ref == "NA"))



##5) Collapse data by id apply unique summarise functions##
ID_grouped <- train%>%
  select(Id, minutes_past,Ref, RefComposite, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(Min_Ref = min(Ref, na.rm = TRUE), 
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

#6) Divide data into >70mm Expected and <70mm Expected to separate true entries from errors
Train_wRef <- filter(ID_grouped, (Expected< 70))


###Building models to predict Expected###
#Build linear model predicting Expected -Ref, RhoHV, Zdr, and Kdp all exist
set.seed(5)
random_sample <- sample(515337, 400000)
lm.fit <- lm(Expected ~mpalmer + Mean_Ref + Ref_spread + Mean_RhoHV + RhoHV_spread + Mean_Zdr + Zdr_spread 
             + Mean_Kdp + Kdp_spread,data = Mean_values_Ref_RhoHV_Zdr_Kdp, 
             subset = random_sample)
#MAE
MAE <- mean(abs(Mean_values_Ref_RhoHV_Zdr_Kdp$Expected - predict(lm.fit, Mean_values_Ref_RhoHV_Zdr_Kdp))[-random_sample])
print(MAE)
summary(lm.fit)

#Build Random Forest predicting Expected - Ref, RhoHV, Zdr, and Kdp all exist
set.seed(5)
forest.ref_impute = randomForest(Expected ~ Mean_Ref + Mean_RhoHV + Mean_Zdr + Mean_Kdp,data = Mean_values_Ref_RhoHV_Zdr_Kdp, subset = random_sample, 
                                 mtry =1, importance = TRUE)
forest.ref_impute
yhat.forest <- predict(forest.ref_impute, newdata = Mean_values_Ref_RhoHV_Zdr_Kdp[-random_sample])
plot(yhat.forest, Mean_values_Ref_RhoHV_Zdr_Kdp[-random_sample]$Expected)
abline(0,1)
#MAE
mean(abs((yhat.forest - Mean_values_Ref_RhoHV_Zdr_Kdp[-random_sample]$Expected)))




#Plot Ref vs. Rho
Mean_values_Ref_RhoHV <- filter(Max_Min_values, (Mean_Ref != "NaN") & (Mean_RhoHV != "NaN"))
Mean_values_Ref_RhoHV %>% ggvis( ~Mean_RhoHV, ~Mean_Ref) %>%
  layer_points()




















