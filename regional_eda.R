# Regional Exploration


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
library(ggrepel)

# General map with nations overlay- anything summarized by nation is fine
# lay on top of that the region points?


loans <- read.csv("kiva_loans.csv", stringsAsFactors = FALSE)
regions <- read.csv("kiva_mpi_region_locations.csv", stringsAsFactors = FALSE)
loans_map <- feather::read_feather("enriched_map_data.feather")
#loan_themes <- read.csv("loan_theme_ids.csv", stringsAsFactors = FALSE)
#loan_themes_region <- read.csv("loan_themes_by_region.csv", stringsAsFactors = FALSE)

loans$any_female <- grepl("female", loans$borrower_genders)

sum_loans_region <- loans %>%
  group_by(country, region) %>%
  summarize(
    loans = n()
    , mean_term = mean(term_in_months, na.rm = T)
    , mean_lenders = mean(lender_count, na.rm = T)
    , mean_amt = mean(loan_amount, na.rm=T)
    , mean_f_term = mean(term_in_months[any_female==1], na.rm = T)
    , mean_f_lenders = mean(lender_count[any_female==1], na.rm = T)
    , mean_f_amt = mean(loan_amount[any_female==1], na.rm = T)
    , gender_ratio_mean_amt = mean_f_amt/mean_amt
  )


loans_regions_coords <- merge(sum_loans_region, regions, by="region", suffixes = c(".country", ".region"))
loans_regions_coords <- loans_regions_coords[loans_regions_coords$region != "",]

# World map with region points
ggplot(loans_regions_coords, aes(x=lon, y=lat, group=country.country))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position="none")+
  #geom_text_repel(data= loans_regions_coords, aes(x=lon ,lat, label = region), size=2)+
  geom_path(data=loans_map, aes(x=long, y=lat, group=group), color="black", size=0.2)+
  geom_point(data=loans_regions_coords, aes(x=lon, y=lat, color = mean_term), size = 1)+
  coord_quickmap()

table(loans_regions_coords$world_region)
table(loans_map$REGION_WB)
head(loans_regions_coords)
loans_regions_coords_filter <- dplyr::filter(loans_regions_coords, world_region == "South Asia")

ggplot(loans_regions_coords_filter, aes(x=lon, y=lat, group=country.country))+
  theme(panel.background=element_rect(fill="white", color="black"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position="none")+
  geom_text_repel(data= loans_regions_coords_filter, aes(x=lon ,lat, label = region), size=2)+
  geom_path(data=loans_map[loans_map$REGION_WB=="South Asia",], aes(x=long, y=lat, group=group), color="black", size=0.2)+
  geom_point(data=loans_regions_coords_filter, aes(x=lon, y=lat, color = mean_term), size = 1)+
  coord_quickmap()
