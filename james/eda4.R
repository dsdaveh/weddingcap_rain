library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)

source('/Users/jamesramadan/Documents/Kaggle/Rain2/R Code')


#summary of train dataset (no nulls)
summary(train)

###Remove any rows with only NAs i.e. if information on one variable is present row stays##
Train_noNAonlys <- filter(train, (Ref != "NA") | (RhoHV != "NA")
                     & (Zdr != "NA") | (Kdp != "NA"))


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






###Summarise by ID###
#Obtain Max
Min_values <- train%>%
  select(Id, Ref, RefComposite, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(Min_Ref = min(Ref, na.rm = TRUE), 
            Min_RefComposite = min(RefComposite, na.rm = TRUE),
            Min_RhoHV = min(RhoHV, na.rm = TRUE),
            Min_Zdr = min(Zdr, na.rm = TRUE),
            Min_Kdp = min(Kdp, na.rm = TRUE),
            Expected = median(Expected)
  )
#Obtain Min
Max_values <- train %>%
  select(Id, Ref, RefComposite, minutes_past, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(Max_Ref = max(Ref, na.rm = TRUE), 
            Mean_Ref = mean(Ref, na.rm = TRUE),
            mpalmer = mpalmer(Ref, minutes_past),
            Max_RefComposite = max(RefComposite, na.rm = TRUE),
            Mean_RefComposite = mean(RefComposite, na.rm= TRUE),
            Max_RhoHV = max(RhoHV, na.rm = TRUE),
            Mean_RhoHV = mean(RhoHV, na.rm=TRUE),
            Max_Zdr = max(Zdr, na.rm = TRUE),
            Mean_Zdr = mean(Zdr, na.rm = TRUE),
            Max_Kdp = max(Kdp, na.rm = TRUE),
            Mean_Kdp = mean(Kdp, na.rm = TRUE))
#Create spread from Max & Min
Max_Min_values <- inner_join(Min_values, Max_values, by = "Id") %>%
  mutate(Ref_spread = Max_Ref - Min_Ref, 
         RefComposite_spread = Max_RefComposite - Min_RefComposite,
         RhoHV_spread = Max_RhoHV - Min_RhoHV,
         Zdr_spread = Max_Zdr - Min_Zdr,
         Kdp_spread = Max_Kdp - Min_Kdp)



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




















