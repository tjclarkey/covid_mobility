library(readr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(plyr)
library(jtools)
library(ggstance)
library(huxtable)

#Read data into R and select only England counties. Use a ddply filter to calculate the difference b/w average pre covid and average post covid moobility by county.
mobdata <- read_csv("~/Documents/Git/covid_mobility/Data/Global_Mobility_Report.csv")
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


# I will change the data frame so that I have entries only for before and after values
tm <- select(
  filter(tm, 
         month == 2 & day == 15 | month == 4 & day ==1,
         !is.na(region)), # this removes the summary for the UK as a whole, averaging over all counties
  before_after, avchg_rec_retail, region)


# Add highest qualification data and enrolment data
educ_data <- read_csv("~/Documents/Git/covid_mobility/Data/U.K./raw-data/Education.csv")
educ_data <- rename(educ_data, 
                    replace = c("GEO_LABEL" = "region",
                                "F186" = "TotalQualResponse",
                                "F187" = "NoQual",
                                "F188" = "L1Qual",
                                "F189" = "L2Qual",
                                "F191" = "L3Qual",
                                "F192" = "L4Qual")
                                ) %>% 
  filter(!is.na(region), GEO_TYP2 == "LA") %>% 
  select(region, TotalQualResponse:L4Qual)

enrol_data <- read_csv("~/Documents/Git/covid_mobility/Data/U.K./raw-data/Enrol.csv")
enrol_data <- rename(enrol_data, 
                     replace = c("GEO_LABEL" = "region",
                                 "F987" = "Enrol_16_17",
                                 "F988" = "Enrol_18_Over")
                     ) %>% 
  filter(GEO_TYP2 == "LA", !is.na(region)) %>%
  select(region, contains("enrol"))
  

main <- left_join(educ_data, enrol_data)


main <- left_join(tm, main) %>% 
  filter(!is.na(NoQual))

#Generate change in mobility variable
main <- main %>% 
  dcast(region ~ before_after, value.var = "avchg_rec_retail") %>% 
  left_join(main) %>% 
  filter(before_after == "after") %>% 
  mutate(change_rec_retail = -(before - after)) %>% 
  select(-(after:avchg_rec_retail))

#Add Population Data
#Rename Variables
popdata <- read_csv("~/Documents/Git/covid_mobility/Data/U.K./raw-data/Population.csv")
popdata <- popdata %>% 
  filter(GEO_TYP2 == "LA") %>% 
  rename(replace = c("GEO_LABEL" = "region",
                     "F2384" = "population")) %>% 
  select(region, population)

main <- left_join(main, popdata)
```

#Rename Variables
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

c <- main %>% 
  select(change_rec_retail, No.Qualifications:College.Grad)

mcor<-round(cor(c),2)
lower2<-mcor
lower2[lower.tri(mcor, diag=TRUE)]<-""
lower2<- as.data.frame(lower2)
lower2


# Use a ddply filter to calculate the difference b/w average pre covid and average post covid moobility by county. Graphic justifying reason for before-after split. 

# I illustrate the approximate beginning of the decline with the following ggplot for rec_retail
# Most counties experience a decrease in mobility significantly below the baseline, 0, around March 14
num <- sample(seq(from = 2, to = 152, by = 1), size = 5, replace = TRUE)
regions <- unique(t$region)
g <-
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
    y = "Percentage Change in Mobility",
    caption = "Figure 1") +
  theme(
    plot.subtitle = element_text(face = "italic", color = "darkblue"),
    plot.caption = element_text(face = "bold")) +
  geom_hline(yintercept = -5, linetype = "longdash", color = "#666666") +
  scale_color_brewer(palette = "Dark2")






#Add Social Grade
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
main <- main %>% 
  mutate(Not.Identified = 1 - (No.Qualifications + Grade.9 + Grade.11 + Grade.12 + College.Grad))




## Empirical Framework
regr <- main %>% 
  select(contains("Qual"), contains("grad"), change_rec_retail, Not.Identified)



reg <- lm(change_rec_retail ~ Not.Identified + Grade.9 + Grade.11 + Grade.12 + College.Grad, data = regr)



export_summs(reg, error_format = "(p = {p.value}, se = {std.error}, CI = [{conf.low}, {conf.high}])")


## Results
#### Validating Endogeneity




maint <- main %>% 
  mutate(
    TotalGSResp = as.numeric(TotalGSResp),
    UpperMiddleP = round(as.numeric(UpperMiddle) / TotalGSResp, 2),
    LowerMiddleP = round(as.numeric(LowerMiddle) / TotalGSResp, 2),
    SkilledWorkingP = round(as.numeric(SkilledWorking) / TotalGSResp, 2),
    Working_NonWorkingP = round(as.numeric(Working_NonWorking) / TotalGSResp, 2)) %>% 
  select(region, change_rec_retail, No.Qualifications:College.Grad, UpperMiddleP:Working_NonWorkingP, Not.Identified, population)






Inc_Mob_Corr <- maint %>% 
  select(change_rec_retail, UpperMiddleP, LowerMiddleP, Working_NonWorkingP, SkilledWorkingP) %>% 
  melt(id = "change_rec_retail") %>% 
  mutate(Social.Class = variable)




ggplot(Inc_Mob_Corr, aes(x = value, y = change_rec_retail, color = Social.Class)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Change in Mobility For Region Composition",
       subtitle = "By Social Class",
       caption = "Figure 2",
       x = "Proportion of Region of Class Type",
       y = "Percentage Change in Mobility") +
  theme(plot.subtitle = element_text(color = "darkblue", face = "italic"),
        plot.caption = element_text(face = "bold"))





groupie <- Inc_Mob_Corr %>% 
  mutate(variable = ifelse(Inc_Mob_Corr$variable == "UpperMiddleP", "HigherClass",
                           ifelse(Inc_Mob_Corr$variable == "LowerMiddleP", "HigherClass", "LowerClass")))


  
  
ggplot(groupie, aes(x = value, y = change_rec_retail, color = variable)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Change in Mobility For Region Composition",
       subtitle = "By Grouped Class",
       caption = "Figure 3",
       x = "Proportion of Region of Class Type",
       y = "Percentage Change in Mobility") +
  theme(plot.subtitle = element_text(color = "darkblue", face = "italic"),
        plot.caption = element_text(face = "bold")) +
  scale_color_brewer(palette = "Dark2")



high <- filter(groupie, variable == "HigherClass")
low <- filter(groupie, variable == "LowerClass")



chigh <- round(cor(high$value, high$change_rec_retail), 3)
clow <- round(cor(low$value, low$change_rec_retail), 3)



QualGrade <-
  maint %>% 
  select(UpperMiddleP, LowerMiddleP, No.Qualifications:College.Grad)



mcor<-round(cor(QualGrade),2)
lower<-mcor
lower[lower.tri(mcor, diag=TRUE)]<-""
lower<- as.data.frame(lower)
lower





  
reg2 <- lm(change_rec_retail ~ Not.Identified + Grade.9 + Grade.11 + Grade.12 + College.Grad + 
             SkilledWorkingP + LowerMiddleP + UpperMiddleP, data = maint)



export_summs(reg, reg2, error_format = "(p = {p.value})")




plot_summs(reg, reg2, plot.distributions = TRUE, scale = TRUE, ci_level = .95, inner_ci_level = .95) +
  labs(title = "Coefficient Confidence as Normal Distributions",
       subtitle = "As an Approximation of the t-Distributions",
       caption = "Figure 4") +
  theme(plot.caption = element_text(face = "bold"))





st <- regr %>%
  select(change_rec_retail, Grade.12, College.Grad) %>% 
  melt(id = "change_rec_retail") %>% 
  rename(c("variable" = "Education.Level"))

ggplot(st, aes(x = value, y = change_rec_retail, color = Education.Level)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Relationship Between Education Level and Change In Mobility",
       subtitle = "Difference Between High School Graduates and College Graduates", 
       x = "Proportion of Population with Given Qualifications",
       y = "Percentage Change in Mobility",
       caption = "Figure 5") +
  theme(plot.subtitle = element_text(color = "darkblue", face = "italic"),
        plot.caption = element_text(face = "bold"))




plot_summs(reg2, reg2, scale = TRUE, robust = list(FALSE, "HC0"),
           model.names = c("OLS", "HC0")) +
  labs(title = "Standard Errors for The Second Model",
       subtitle = "With and without robust standard errors",
       caption = "Figure 6") +
  theme(plot.caption = element_text(face = "bold"))
