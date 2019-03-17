
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

# Loan Data Cleaning and Conversion

loans <- read.csv("data/kiva_loans.csv", stringsAsFactors = FALSE)

#regions <- read.csv("kiva_mpi_region_locations.csv", stringsAsFactors = FALSE)
#loan_themes <- read.csv("loan_theme_ids.csv", stringsAsFactors = FALSE)
#loan_themes_region <- read.csv("loan_themes_by_region.csv", stringsAsFactors = FALSE)

loans$any_female <- grepl("female", loans$borrower_genders)


# Currency Conversions
loans$currency_quote_string <- paste0(loans$currency, "USD=X")
lookup_exchanges <- unique(loans$currency_quote_string)
lookup_exchanges <- lookup_exchanges[!(lookup_exchanges %in% c("ZWDUSD=X", "SSPUSD=X"))]
rates <- quantmod::getQuote(lookup_exchanges)[2]
rates <- rbind(rates, `ZWDUSD=X` = 0.00276319, `SSPUSD=X` = 0.0077)

#zimbabwean obsolete currency: ZWD: 0.00276319
#south sudanese pound: SSP: 0.0077

loans <- loans %>%
  mutate(usd_amt = rates[loans$currency_quote_string, "Last"] * loans[, "loan_amount"])




