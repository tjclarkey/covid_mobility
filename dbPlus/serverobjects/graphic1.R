# This is the code which produces graphic1

library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(plyr)
library(jtools)
library(ggstance)
library(huxtable)


# Reads data. Removes census_fips_code column.         
mobdata <- read_csv("~/Documents/Git/covid_mobility/Data/MobRep.csv", 
                    col_types = cols(sub_region_2 = col_character()))
mobdata <- select(mobdata, -(census_fips_code))

# Function of x - country name, and a - type of mobility.
graphic1 <- function(x, a) {
  
  s <-
    filter(mobdata, country_region == x) %>% 
    rename(replace = 
             c("country_region_code" = "country_code",
               "country_region" = "country",
               "sub_region_1" = "Region",
               "sub_region_2" = "sub_region",
               "retail_and_recreation_percent_change_from_baseline" = "pc_Recreation_and_Retail",
               "grocery_and_pharmacy_percent_change_from_baseline"  = "pc_Groceries_and_Pharmacies",
               "parks_percent_change_from_baseline" = "pc_Parks",
               "transit_stations_percent_change_from_baseline" = "pc_Transit_Stations",
               "workplaces_percent_change_from_baseline" = "pc_Workplaces",
               "residential_percent_change_from_baseline" = "pc_Residential")) %>% 
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           day = lubridate::day(date))
  
  b <- paste("pc", a, sep = "_")

  regions <- unique(s$Region)
  num <- sample(seq(from = 2, to = length(regions), by = 1), size = 5, replace = TRUE)
  
  ggplot(filter(s, 
                Region == regions[num[1]] |
                  Region == regions[num[2]] | 
                  Region == regions[num[3]] | 
                  Region == regions[num[4]] | 
                  Region == regions[num[5]])) +
    geom_smooth(aes_string(x = "date", y = b, color = "Region"), se = FALSE) +
    labs(
      title = paste("Mobility Over Time In", x, sep = " "),
      subtitle = paste("For", gsub("_", " ", a), sep = " "),
      x = "Date",
      y = paste("Percentage Change in", gsub("_", " ", a), "Mobility", sep = " ")) +
    theme(
      plot.subtitle = element_text(face = "italic", color = "darkblue")) +
    geom_hline(yintercept = -5, linetype = "longdash", color = "#666666") +
    scale_color_brewer(palette = "Dark2")
  
}
