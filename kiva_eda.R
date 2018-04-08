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

source("kiva_loan_cleaning.R")

# Summarize
sum_loans <- loans %>%
  group_by(country) %>%
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


source("gis.R")

loans_map <- merge(sum_loans, shapefile1_df2, by.x = "country", by.y="NAME_CIAWF", all.y = T)
loans_map <- arrange(loans_map, order)

feather::write_feather(loans_map, "Loans/enriched_map_data.feather")

###
#loans_map <- feather::read_feather("enriched_map_data.feather")

country_names <- aggregate(cbind(long, lat) ~ country, data=loans_map, 
                    FUN=function(x)mean(range(x)))
loans_map2 <- merge(loans_map[,c("country", "CONTINENT", "ECONOMY", "INCOME_GRP")], country_names, by="country", all = T, suffixes = c(".plain", ".centroid"))
loans_map2 <- unique(loans_map2)

feather::write_feather(loans_map2, "Loans/country_names.feather")

rm(shapefile1)
rm(shapefile1_df)
