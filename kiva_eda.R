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


# Summarize
sum_loans <- loans %>%
  group_by(country) %>%
  summarize(
    mean_term = mean(term_in_months, na.rm = T)
    , mean_lenders = mean(lender_count, na.rm = T)
    , mean_amt = mean(loan_amount, na.rm=T)
  )


source("gis.R")

loans_map <- merge(sum_loans, shapefile1_df2, by.x = "country", by.y="NAME_CIAWF", all.y = T)
loans_map <- arrange(loans_map, order)

feather::write_feather(loans_map, "enriched_map_data.feather")

###
#loans_map <- feather::read_feather("enriched_map_data.feather")

africa_map <- filter(loans_map, CONTINENT == "Africa")
asia_map <- filter(loans_map, CONTINENT == "Asia")
sa_map <- filter(loans_map, CONTINENT == "South America")

rm(shapefile1)
rm(shapefile1_df)
