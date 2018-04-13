# Using new region data, produce maps

# Kiva data project - EDA

library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(dplyr)
library(sp)
library(spdep)
library(raster)
library(RColorBrewer)
library(maptools)
library(classInt)
library(broom)
library(quantmod)

source("~/kaggle_kiva/scripts/kiva_loan_cleaning.R")

region_merge <- feather::read_feather("~/kaggle_kiva/data/region_merge.feather")
region_merge <- unique(region_merge[, c('LocationName','ISO','country.x', 'region.x',  'world_region'
                                        , 'MPI', 'geo', 'lat','lon', 'region_low','id')])

sumrm <- region_merge %>%
  group_by(id) %>%
  summarize(records = n()) %>%
  ungroup() %>%
  filter(records > 1)

region_merge[region_merge$id == 653098,]

region_loans <- merge(loans, region_merge, by="id")

head(region_merge, 5)

# Summarize
sum_loans <- region_loans %>%
  group_by(country, region) %>%
  summarize(
    total_loans = n()
    , mean_term = mean(term_in_months, na.rm = T)
    , mean_lenders = mean(lender_count, na.rm = T)
    , mean_amt = mean(usd_amt, na.rm=T)
    , median_amt = median(usd_amt, na.rm=T)
    , mean_f_term = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt = mean(usd_amt[any_female==1], na.rm = T)
    , median_f_amt = median(usd_amt[any_female==1], na.rm = T)
    , gender_ratio_mean_amt = mean_f_amt/mean_amt
    , gender_ratio_median_amt = median_f_amt/median_amt
  )