library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)
library(magrittr)

source("/Users/jamesramadan/Documents/Kaggle/Rain2/R Code/data-prep.R")
source("/Users/jamesramadan/Documents/Kaggle/Rain2/R Code/rain_utils.R")
tcheck(0)

#summary of train dataset (no nulls)
summary(train)

#Filter out >305mm Expected. Also, an outlier with Kdp ~80.
train_low <- filter(train.sample1000, Expected <=70 & Kdp <60)

#scatterplots with sample dataset
train.sample1000 %>% ggvis( ~radardist_km, ~Expected) %>%
  layer_points()

train.sample1000 %>% ggvis( ~Ref, ~Expected) %>%
  layer_points()

train.sample1000 %>% ggvis( ~RefComposite, ~Expected) %>%
  layer_points()

train.sample1000 %>% ggvis( ~RhoHV, ~Expected) %>%
  layer_points()

train.sample1000 %>% ggvis( ~Zdr, ~Expected) %>%
  layer_points()

train.sample1000 %>% ggvis( ~Kdp, ~Expected) %>%
  layer_points()


#scatterplots with sample dataset. No expected > 70
train_low %>% ggvis( ~radardist_km, ~Expected) %>%
  layer_points()

train_low %>% ggvis( ~Ref, ~Expected) %>%
  layer_points()

train_low %>% ggvis( ~RefComposite, ~Expected) %>%
  layer_points()

train_low %>% ggvis( ~RhoHV, ~Expected) %>%
  layer_points()

train_low %>% ggvis( ~Zdr, ~Expected) %>%
  layer_points()

train_low %>% ggvis( ~Kdp, ~Expected) %>%
  layer_points()
