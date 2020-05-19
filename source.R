After this create a general function of all previous manipulation so that 
the same can be done for each different type of movement

Now I want to add all other types of mobility change for which there are
significance. E.g. ignore types of changes where there is seldom insufficient data to 
produce day by day results - check the google pdf.

Then I want to also add to the dataframe my control variables / 
  possible instruments and perform an additional regression. Before doing this 
I will check MLR.1 - MLR.5. I will also consider the functional form of my initial
model and the secondary model. 


Ideas / Questions:
  
  It might not be surprising if the average drop in mobility is not hugely different.
Societies tend to adjust eventually, after all. What may be more interesting is inspecting 
how the rate of change of mobility, i.e. how quickly different counties reached the average low-point,
differs amongst counties.









library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(plyr)
library(jtools)
library(ggstance)
library(huxtable)

mobdata <- read_csv("~/Documents/R/107/Data/Global_Mobility_Report.csv")
t <- filter(mobdata, country_region == "United Kingdom") %>% 
  select(-(sub_region_2)) %>% 
  rename(replace = c(
    "country_region_code" = "country_code",
    "country_region" = "country",
    "sub_region_1" = "region",
    "retail_and_recreation_percent_change_from_baseline" = "pc_rec_retail",
    "grocery_and_pharmacy_percent_change_from_baseline"  = "pc_grocery_pharma",
    "parks_percent_change_from_baseline" = "pc_parks",
    "transit_stations_percent_change_from_baseline" = "pc_transit",
    "workplaces_percent_change_from_baseline" = "pc_workplace",
    "residential_percent_change_from_baseline" = "pc_residential")) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))

# I illustrate the approximate beginning of the decline with the following ggplot for rec_retail
# Most counties experience a decrease in mobility significantly below the baseline, 0, around March 14
num <- sample(seq(from = 2, to = 152, by = 1), size = 5, replace = TRUE)
regions <- unique(t$region)
ggplot(filter(t, 
              region == regions[num[1]] |
                region == regions[num[2]] | 
                region == regions[num[3]] | 
                region == regions[num[4]] | 
                region == regions[num[5]])) +
  geom_smooth(aes(x = date, y = pc_rec_retail, color = region), se = FALSE) +
  labs(
    title = "Change in Mobility for 5 random counties",
    subtitle = "This change is for the Recreation and Retail category",
    x = "Date",
    y = "Percentage Change in Mobility"
  ) +
  theme(
    plot.subtitle = element_text(face = "italic", color = "darkblue")
  ) +
  geom_hline(yintercept = -5, linetype = "longdash", color = "#ff4f42") +
  scale_color_brewer(palette = "Dark2")


# Create a column which indicates whether the entry corresponds to before the news of potential lockdown and after the news of the lockdown.
# I am  using March 14 as start date. The official lockdown announcement was March 23, but there was a drop in movement before then.
for (i in unique(t$month)) {
  for (j in unique(t$day)) {
    t[, "before_after"] <- ifelse(t$month == 2 | (t$month == 3 & t$day <= 14), "before", "after")
  }
}

# Now I will find the average for each county, both for before and after the change. Each entry will have an additional column
# gives the average change for that county.
av <- function(x) {
  mean(x, na.rm = TRUE)
}
tm <- ddply(t, .(region, before_after), transform, avchg_rec_retail = av(pc_rec_retail))

tm <- select(
  filter(tm, 
         month == 2 & day == 15 | month == 4 & day ==1,
         !is.na(region)), # this removes the summary for the UK as a whole, averaging over all counties
  before_after, avchg_rec_retail, region)

# Row 97 and 106 remove rows which denote regions smaller than counties. In some cases, there are repeats                                 # in region names between the Local Authority and Wards and Divisions levels. We only want                                                # data for the county level.
educ_data <- read_csv("~/Documents/R/107/Data/Education.csv")
educ_data <- rename(educ_data, 
                    replace = c("GEO_LABEL" = "region",
                                "F186" = "TotalQualResponse",
                                "F187" = "NoQual",
                                "F188" = "L1Qual",
                                "F189" = "L2Qual",
                                "F191" = "L3Qual",
                                "F192" = "L4Qual")) %>% 
  filter(!is.na(region), GEO_TYP2 == "LA") %>% 
  select(region, TotalQualResponse:L4Qual)

enrol_data <- read_csv("~/Documents/R/107/Data/Enrol.csv")
enrol_data <- rename(enrol_data, 
                     replace = c("GEO_LABEL" = "region",
                                 "F987" = "Enrol_16_17",
                                 "F988" = "Enrol_18_Over")) %>% 
  filter(GEO_TYP2 == "LA", !is.na(region)) %>%
  select(region, contains("enrol"))


main <- left_join(educ_data, enrol_data)


main <- left_join(tm, main) %>% 
  filter(!is.na(NoQual))

main <- main %>% 
  dcast(region ~ before_after, value.var = "avchg_rec_retail") %>% 
  left_join(main) %>% 
  filter(before_after == "after") %>% 
  mutate(change_rec_retail = -(before - after)) %>% 
  select(-(after:avchg_rec_retail))


popdata <- read_csv("~/Documents/R/107/Data/Population.csv")
popdata <- popdata %>% 
  filter(GEO_TYP2 == "LA") %>% 
  rename(replace = c("GEO_LABEL" = "region",
                     "F2384" = "population")) %>% 
  select(region, population)

main <- left_join(main, popdata)
main <- 
  mutate(main, 
         population = as.numeric(main$population),
         NoQual = as.numeric(main$NoQual),
         L1Qual = as.numeric(main$L1Qual),
         L2Qual = as.numeric(main$L2Qual),
         L3Qual = as.numeric(main$L3Qual),
         L4Qual = as.numeric(main$L4Qual),
         Enrol_16_17 = as.numeric(main$Enrol_16_17),
         Enrol_18_Over = as.numeric(main$Enrol_18_Over),
         NoQual_pc = round(NoQual / as.numeric(TotalQualResponse), 4),
         L1Qual_pc = round(L1Qual / as.numeric(TotalQualResponse), 4),
         L2Qual_pc = round(L2Qual / as.numeric(TotalQualResponse), 4),
         L3Qual_pc = round(L3Qual / as.numeric(TotalQualResponse), 4),
         L4Qual_pc = round(L4Qual / as.numeric(TotalQualResponse), 4)) %>% 
  rename(c("L3Qual_pc" = "Grade.12",
           "L4Qual_pc" = "College.Grad",
           "L2Qual_pc" = "Grade.11",
           "L1Qual_pc" = "Grade.9",
           "NoQual_pc" = "No.Qualifications"))




social_grade <- read_csv("~/Documents/R/107/Data/SocialGrade.csv")
social_grade <- 
  rename(social_grade, 
         replace = c("GEO_LABEL" = "region",
                     "F323271" = "UpperMiddle",
                     "F323272" = "LowerMiddle",
                     "F323273" = "SkilledWorking",
                     "F323274" = "Working_NonWorking",
                     "F323270" = "TotalGSResp")
  ) %>% 
  filter(!is.na(region), GEO_TYP2 == "LA") %>% 
  select(region, TotalGSResp:Working_NonWorking)

main <- left_join(main, social_grade)


# Other qualifications: Vocational/Work-related Qualifications, Foreign Qualifications (Not stated/ level unknown).
regr <- main %>% 
  select(contains("Qual"), contains("grad"), change_rec_retail) %>% 
  mutate(Not.Identified = 1 - (No.Qualifications + Grade.9 + Grade.11 + Grade.12 + College.Grad))

reg <- lm(change_rec_retail ~ Not.Identified + Grade.9 + Grade.11 + Grade.12 + College.Grad, data = regr)
summary(reg)
confint(reg)

export_summs(reg)
# Think about the story of age. Relationship between L4Qual and older individuals? May this result in smaller change for this educaiton group - age is another potential factor which could decrease the usefulness of a response; more ignorant, care less, lower ability to receive communication.

st <- regr %>% 
  rename(c("L3Qual_pc" = "High.School.Grad",
           "L4Qual_pc" = "College.Grad")) %>% 
  select(change_rec_retail, High.School.Grad, College.Grad) %>% 
  melt(id = "change_rec_retail") %>% 
  rename(c("variable" = "Education.Level"))

ggplot(st, aes(x = value, y = change_rec_retail, color = Education.Level)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Relationship Between Education Level and Change In Mobility",
       subtitle = "Difference Between High School Graduates and College Graduates", 
       x = "Proportion of Population with Given Qualifications",
       y = "Percentage Change in Mobility") +
  theme(plot.subtitle = element_text(color = "darkblue", face = "italic"))
