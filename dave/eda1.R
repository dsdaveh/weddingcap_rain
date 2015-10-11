library(data.table)
library(Metrics)
library(dplyr)
library(tidyr)
library(ggvis)

source( "../team/data-prep.R")
source("../team/rain_utils.R")
tcheck(0)

refs <- train %>%
    select( starts_with("Ref") ) %>%
    gather( Refxxx, Value, starts_with("Ref")) %>%
    group_by(Refxxx)

refs %>% ggvis( ~Value) %>%
    layer_densities( fill = ~Refxxx, density_args = list( na.rm=TRUE))

rm(refs); tcheck()

rhos <- train %>%
    select( starts_with("Rho") ) %>%
    gather( Rhoxxx, Value, starts_with("Rho")) %>%
    group_by(Rhoxxx)rm

rhos %>% ggvis( ~Value) %>%
    layer_densities( fill = ~Rhoxxx, density_args = list( na.rm=TRUE))

rm(rhos); tcheck()

zdrs <- train %>%
    select( starts_with("Zdr") ) %>%
    gather( Zdrxxx, Value, starts_with("Zdr")) %>%
    group_by(Zdrxxx)

zdrs %>% ggvis( ~Value) %>%
    layer_densities( fill = ~Zdrxxx, density_args = list( na.rm=TRUE))

rm(zdrs); tcheck()

kdps <- train %>%
    select( starts_with("Kdp") ) %>%
    gather( Kdpxxx, Value, starts_with("Kdp")) %>%
    group_by(Kdpxxx)

kdps %>% ggvis( ~Value) %>%
    layer_densities( fill = ~Kdpxxx, density_args = list( na.rm=TRUE))

rm(kdps); tcheck()
tcheck(4)