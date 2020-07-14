# The function main_function takes in a country name and mobility type and manipulates the data so that
# it can readily have a regression performed on it or calculate average mobility change

library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(plyr)
library(jtools)
library(ggstance)
library(huxtable)

mobdata <- read_csv("~/Documents/Git/covid_mobility/Data/MobRep.csv", 
                    col_types = cols(sub_region_2 = col_character()))
mobdata <- select(mobdata, -(census_fips_code))

countries <- unique(mobdata$country_region)


# x is the country name
# a is the type of mobility. if the type of mobility is recreation and retail, then a == "p\c_rec_retail".
main_function <- function(x, a) { 
  
  # Allows the user to select a country to inspect and this function will filter for that specific country
  # Function input - x, x: country name
  
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
  
  
  # Assigns dates to either before pandemic policy announcement or after. At some point I will allow the dates to be inputs, or at least vary for different countries / regions
  t <- s
  for (i in unique(t$month)) {
    for (j in unique(t$day)) {
      t[, "before_after"] <- ifelse(s$month == 2 | (s$month == 3 & s$day <= 14), "before", "after")
    }
  }
  # t has change in mobility for each type and for every day for the chosen country
  
  
  # The below if else set does the following:
  # - something: creates a variable for the mean change before and after at the country, county, and sub region level
  # - something2: data frame with only two rows for each region (sub_region); average mobility before and average after.
  #               this is useful for if only knowing the pre and post averages is useful
  # - something3: creates a variable which is the change in mobility for each region
  # - something4: is the same as something3 but for sub_region
  #               - only happens if data exists at a level smaller than the region level
  
  # if the two objects below are equal, then there are no sub_regions for a country
  numberofNAs <- count(is.na(select(filter(t, country == x), sub_region)))$freq
  numberofRows <- count(select(filter(t, country == x), sub_region))$freq
  
  
  if (a == "rec_retail") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       rec_retail_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        rec_retail_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        rec_retail_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "rec_retail_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_rec_retail_county1 = -(before - after))
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "rec_retail_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_rec_retail_county2 = -(before - after))    
    }
  } 
  
  else if (a == "grocery_pharma") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       grocery_pharma_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        grocery_pharma_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        grocery_pharma_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "grocery_pharma_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_grocery_pharma_county1 = -(before - after))
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "grocery_pharma_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_grocery_pharma_county2 = -(before - after))
    }
  }
  
  else if (a == "parks") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       parks_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        parks_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        parks_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "parks_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_parks_county1 = -(before - after))
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "parks_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_parks_county2 = -(before - after))
    }
  }
  
  else if (a == "transit") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       transit_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        transit_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        transit_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "transit_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_transit_county1 = -(before - after))
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "transit_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_transit_county2 = -(before - after))    
    }
  }
  
  else if (a == "workplace") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       workplace_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        workplace_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        workplace_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "workplace_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_workplace_county1 = -(before - after))
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "workplace_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_workplace_county2 = -(before - after))
    }
  }
  
  else if (a == "residential") {
    something <- ddply(t, 
                       .(before_after), 
                       transform, 
                       residential_country = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply( 
        .(region, before_after),
        transform,
        residential_county1 = mean(pc_rec_retail, na.rm = TRUE)) %>% 
      ddply(
        .(region, sub_region, before_after),
        transform,
        residential_county2 = mean(pc_rec_retail, na.rm = TRUE))
    
    something2 <- select(filter(something, 
                                month == 2 & day == 15 | month == 4 & day ==1, !is.na(region)), 
                         -(pc_rec_retail:pc_residential),
                         -(contains(c("iso", "census")))
    )
    
    something3 <- something2 %>% 
      dcast(region ~ before_after, value.var = "residential_county1") %>% 
      left_join(something2) %>% 
      filter(before_after == "after") %>% 
      mutate(change_rec_retail = -(before - after))
    
    
    if(numberofNAs == numberofRows) {
      something3 <- select(something3, -(sub_region))
    } else {
      something4 <- something2 %>% 
        dcast(sub_region ~ before_after, value.var = "residential_county2") %>% 
        left_join(something2) %>% 
        filter(before_after == "after") %>% 
        mutate(change_residential_county2 = -(before - after))
    }
  }
  
}




