# This is the code which produces graphic2

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
graphic2 <- function(x, a) {
  
  s <-
    filter(mobdata, country_region == x) %>% 
    rename(replace = 
             c("country_region_code" = "country_code",
               "country_region" = "country",
               "sub_region_1" = "region",
               "sub_region_2" = "sub_region",
               "retail_and_recreation_percent_change_from_baseline" = "pc_rec_retail",
               "grocery_and_pharmacy_percent_change_from_baseline"  = "pc_grocery_pharma",
               "parks_percent_change_from_baseline" = "pc_parks",
               "transit_stations_percent_change_from_baseline" = "pc_transit",
               "workplaces_percent_change_from_baseline" = "pc_workplace",
               "residential_percent_change_from_baseline" = "pc_residential")) %>% 
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           day = lubridate::day(date))
  
  b <- paste("pc", a, sep = "_")
  
  regions <- unique(s$sub_region)
  num <- sample(seq(from = 2, to = length(regions), by = 1), size = 5, replace = TRUE)
  ggplot(filter(s, 
                sub_region == regions[num[1]] |
                  sub_region == regions[num[2]] | 
                  sub_region == regions[num[3]] | 
                  sub_region == regions[num[4]] | 
                  sub_region == regions[num[5]])) +
    geom_smooth(aes_string(x = "date", y = b, color = "sub_region"), se = FALSE, stat) +
    labs(
      title = "Change in Mobility for 5 random sub_regions",
      subtitle = paste("This change is for the", "Chosen Mobility", "category", sep = " "),
      x = "Date",
      y = "Percentage Change in Mobility") +
    theme(
      plot.subtitle = element_text(face = "italic", color = "darkblue")) +
    geom_hline(yintercept = -5, linetype = "longdash", color = "#666666") +
    scale_color_brewer(palette = "Dark2")
  
}