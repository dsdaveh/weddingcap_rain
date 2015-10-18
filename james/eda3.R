library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)

source("../team/data-prep.R")
source("../team/rain_utils.R")
tcheck(0)

#summary of train dataset (no nulls)
summary(train)


###Examining trend over time: Variability in values per ID###
#Obtain Max
Min_values <- train.sample1000 %>%
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
Max_values <- train.sample1000 %>%
  select(Id, Ref, RefComposite, RhoHV, Zdr, Kdp, Expected) %>%
  group_by(Id) %>%
  summarise(Max_Ref = max(Ref, na.rm = TRUE), 
            Max_RefComposite = max(RefComposite, na.rm = TRUE),
            Max_RhoHV = max(RhoHV, na.rm = TRUE),
            Max_Zdr = max(Zdr, na.rm = TRUE),
            Max_Kdp = max(Kdp, na.rm = TRUE))
#Create spread from Max & Min
Max_Min_values <- inner_join(Min_values, Max_values, by = "Id") %>%
  mutate(Ref_spread = Max_Ref - Min_Ref, 
         RefComposite_spread = Max_RefComposite - Min_RefComposite,
         RhoHV_spread = Max_RhoHV - Min_RhoHV,
         Zdr_spread = Max_Zdr - Min_Zdr,
         Kdp_spread = Max_Kdp - Min_Kdp)
#Remove Null  
Spread_values_Ref <- filter(Max_Min_values, (Ref_spread != "-Inf"))
Spread_values_RefComp <- filter(Max_Min_values, (RefComposite_spread != "-Inf"))
Spread_values_RhoHV <- filter(Max_Min_values, (RhoHV_spread != "-Inf"))
Spread_values_Zdr <- filter(Max_Min_values, (Zdr_spread != "-Inf"))
Spread_values_Kdp <- filter(Max_Min_values, (Kdp_spread != "-Inf") & (Kdp_spread <80))

#Plot values vs. Expected
Spread_values_Ref %>% ggvis( ~Ref_spread, ~Expected) %>%
  layer_points()
Spread_values_RefComp %>% ggvis( ~RefComposite_spread, ~Expected) %>%
  layer_points()
Spread_values_RhoHV %>% ggvis( ~RhoHV_spread, ~Expected) %>%
  layer_points()
Spread_values_Zdr %>% ggvis( ~Zdr_spread, ~Expected) %>%
  layer_points()
Spread_values_Kdp %>% ggvis( ~Kdp_spread, ~Expected) %>%
  layer_points()

#Remove >70 Expected
Spread_values_Ref_low <- filter(Max_Min_values, (Ref_spread != "-Inf") & Expected < 70)
Spread_values_RefComp_low <- filter(Max_Min_values, (RefComposite_spread != "-Inf") & Expected <70)
Spread_values_RhoHV_low <- filter(Max_Min_values, (RhoHV_spread != "-Inf") & Expected < 70)
Spread_values_Zdr_low <- filter(Max_Min_values, (Zdr_spread != "-Inf") & Expected < 70)
Spread_values_Kdp_low <- filter(Max_Min_values, (Kdp_spread != "-Inf") & (Kdp_spread <80) & Expected<70) 

#plot vs. Expected (Only <70 Expected)
Spread_values_Ref_low %>% ggvis( ~Ref_spread, ~Expected) %>%
  layer_points()
Spread_values_RefComp_low %>% ggvis( ~RefComposite_spread, ~Expected) %>%
  layer_points()
Spread_values_RhoHV_low %>% ggvis( ~RhoHV_spread, ~Expected) %>%
  layer_points()
Spread_values_Zdr_low %>% ggvis( ~Zdr_spread, ~Expected) %>%
  layer_points()
Spread_values_Kdp_low %>% ggvis( ~Kdp_spread, ~Expected) %>%
  layer_points()





###Examining trend over time: Delta in values (Beginning to End) per ID ###
#Obtain Beginning values
Beginning_minutes <- train.sample1000 %>%
  select(Id, minutes_past) %>%
  group_by(Id) %>%
  summarise(minutes_past = min(minutes_past))

Beginning_values <- inner_join(Beginning_minutes, train.sample1000, 
                               by = c("Id" = "Id", "minutes_past" = "minutes_past")) %>%
  select(Id, minutes_past, Ref, RefComposite, RhoHV, Zdr, Kdp, Expected)

#Obtain End values
End_minutes <- train.sample1000 %>%
  select(Id, minutes_past) %>%
  group_by(Id) %>%
  summarise(minutes_past = max(minutes_past))

End_values <- inner_join(End_minutes, train.sample1000, by = c("Id" = "Id", "minutes_past" = "minutes_past")) %>%
  select(Id, minutes_past, Ref, RefComposite, RhoHV, Zdr, Kdp)

# Create Delta: End - Beginning Values
Beg_End_values <- inner_join(Beginning_values, End_values, by = "Id") %>%
  mutate(Ref_delta = Ref.y - Ref.x, 
         RefComposite_delta = RefComposite.y - RefComposite.x,
         RhoHV_delta = RhoHV.y - RhoHV.x,
         Zdr_delta = Zdr.y - Zdr.x,
         Kdp_delta = Kdp.y - Kdp.x)


#Remove Null  
Delta_values_Ref <- filter(Beg_End_values, (Ref_delta != "NA"))
Delta_values_RefComp <- filter(Beg_End_values, (RefComposite_delta != "NA"))
Delta_values_RhoHV <- filter(Beg_End_values, (RhoHV_delta != "NA"))
Delta_values_Zdr <- filter(Beg_End_values, (Zdr_delta != "NA"))
Delta_values_Kdp <- filter(Beg_End_values, (Kdp_delta != "NA"))

#Plot values vs. Expected
Delta_values_Ref %>% ggvis( ~Ref_delta, ~Expected) %>%
  layer_points()
Delta_values_RefComp %>% ggvis( ~RefComposite_delta, ~Expected) %>%
  layer_points()
Delta_values_RhoHV %>% ggvis( ~RhoHV_delta, ~Expected) %>%
  layer_points()
Delta_values_Zdr %>% ggvis( ~Zdr_delta, ~Expected) %>%
  layer_points()
Delta_values_Kdp %>% ggvis( ~Kdp_delta, ~Expected) %>%
  layer_points()

#Remove Null  & <70 Expected
Delta_values_Ref_low <- filter(Beg_End_values, (Ref_delta != "NA") & Expected <70)
Delta_values_RefComp_low <- filter(Beg_End_values, (RefComposite_delta != "NA") & Expected <70)
Delta_values_RhoHV_low <- filter(Beg_End_values, (RhoHV_delta != "NA") & Expected <70)
Delta_values_Zdr_low <- filter(Beg_End_values, (Zdr_delta != "NA") & Expected <70)
Delta_values_Kdp_low <- filter(Beg_End_values, (Kdp_delta != "NA") & Expected <70)

#Plot values vs. Expected (Only <70 Expected)
Delta_values_Ref_low %>% ggvis( ~Ref_delta, ~Expected) %>%
  layer_points()
Delta_values_RefComp_low %>% ggvis( ~RefComposite_delta, ~Expected) %>%
  layer_points()
Delta_values_RhoHV_low %>% ggvis( ~RhoHV_delta, ~Expected) %>%
  layer_points()
Delta_values_Zdr_low %>% ggvis( ~Zdr_delta, ~Expected) %>%
  layer_points()
Delta_values_Kdp_low %>% ggvis( ~Kdp_delta, ~Expected) %>%
  layer_points()
