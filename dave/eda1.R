library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)

source( "../team/data-prep.R")

refs <- train %>%
  select( starts_with("Ref") ) %>%
  gather( Refxxx, Value, starts_with("Ref")) %>%
  group_by(Refxxx)

refs %>% ggvis( ~Value) %>%
  layer_densities( fill = ~Refxxx, density_args = list( na.rm=TRUE))

rm(refs)

rhos <- train %>%
  select( starts_with("Rho") ) %>%
  gather( Rhoxxx, Value, starts_with("Rho")) %>%
  group_by(Rhoxxx)

rhos %>% ggvis( ~Value) %>%
  layer_densities( fill = ~Rhoxxx, density_args = list( na.rm=TRUE))