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

# General map with nations overlay- anything summarized by nation is fine
# lay on top of that the region points?


loans <- read.csv("kiva_loans.csv", stringsAsFactors = FALSE)
#regions <- read.csv("kiva_mpi_region_locations.csv", stringsAsFactors = FALSE)
#loan_themes <- read.csv("loan_theme_ids.csv", stringsAsFactors = FALSE)
#loan_themes_region <- read.csv("loan_themes_by_region.csv", stringsAsFactors = FALSE)

loans$any_female <- grepl("female", loans$borrower_genders)

# Summarize by country
sum_loans <- loans %>%
  group_by(country) %>%
  summarize(
    mean_term.plain = mean(term_in_months, na.rm = T)
    , mean_lenders.plain = mean(lender_count, na.rm = T)
    , mean_amt.plain = mean(loan_amount, na.rm=T)
    , mean_f_term.plain = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders.plain = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt.plain = mean(loan_amount[any_female==1], na.rm = T)
  )

# by region
sum_loans_region <- loans %>%
  group_by(country, region) %>%
  summarize(
    mean_term.region = mean(term_in_months, na.rm = T)
    , mean_lenders.region = mean(lender_count, na.rm = T)
    , mean_amt.region = mean(loan_amount, na.rm=T)
    , mean_f_term.region = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders.region = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt.region = mean(loan_amount[any_female==1], na.rm = T)
  )

# Summarize by country and sector of loan
sum_loans_sector <- loans %>%
  group_by(country, sector) %>%
  summarize(
    mean_term.sector = mean(term_in_months, na.rm = T)
    , mean_lenders.sector = mean(lender_count, na.rm = T)
    , mean_amt.sector = mean(loan_amount, na.rm=T)
    , mean_f_term.sector = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders.sector = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt.sector = mean(loan_amount[any_female==1], na.rm = T)
  )

# Summarize by country and activity of loan
sum_loans_activity <- loans %>%
  group_by(country, activity) %>%
  summarize(
    mean_term.activity = mean(term_in_months, na.rm = T)
    , mean_lenders.activity = mean(lender_count, na.rm = T)
    , mean_amt.activity = mean(loan_amount, na.rm=T)
    , mean_f_term.activity = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders.activity = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt.activity = mean(loan_amount[any_female==1], na.rm = T)
  )

source("gis.R")

loans_map <- merge(sum_loans, shapefile1_df2, by.x = "country", by.y="NAME_CIAWF", all = T)
loans_map <- merge(loans_map, sum_loans_activity, by.x = "country", by.y="country", all = T, suffixes = c(".plain", ".activity"))
loans_map <- merge(loans_map, sum_loans_sector, by.x = "country", by.y="country", all = T, suffixes = c(".plain", ".sector"))

loans_map <- arrange(loans_map, order)

feather::write_feather(loans_map, "enriched_map_data.feather")

###
#loans_map <- feather::read_feather("enriched_map_data.feather")

africa_map <- filter(loans_map, CONTINENT == "Africa")
asia_map <- filter(loans_map, CONTINENT == "Asia")
sa_map <- filter(loans_map, CONTINENT == "South America")

rm(shapefile1)
rm(shapefile1_df)
