# initial EDA for Kaggle rain
library(lattice)

#load and prep the data if this hasn't been done
source("../team/data-prep.R")

#TODO:  actual EDA
# - correlations (various)
# - correlation of Ref/Expected for different bins of Rho --> are there better correlations
#   for certain value of Rho?


train.collapsed <- train %>% group_by(Id)
                    
  
#  train %>% group_by(Id) %>% 
#  summarise( V1 = mean(Expected)) %>% 
#  summarise( median(V1))
