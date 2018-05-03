library(tidyverse)
library(mapdata)

import <- read_csv("df_scrape_incl_vegas.csv")
import <- read_csv("Mother Jones' Investigation_ US Mass Shootings, 1982-2018 - US mass shootings.csv")

import$assault <- str_detect(import$`Weapon details`, rifle_ar15_regex)
import$handgun <- str_detect(import$`Weapon details`, handgun_regex)

assault <- import[import$assault,] %>% filter(!is.na(import$`Total victims`))
handgun <- import[import$handgun,]
t.test(assault$`Total victims`, handgun$`Total victims`)

congress_clean <- DGT %R% optional(DGT)

import$congresional_district <- str_extract(test, congress_clean)
import$assault_rifle <- (!is.na(import$assault_weapon) | !is.na(import$rifle))
import$handgun <- (!is.na(import$handgun_general) | !is.na(import$handgun_specific))


df_clean <- import %>%
        select(date:city, geolocation:congresional_district, assault_rifle:handgun)


df_clean_assault <- subset(df_clean, assault_rifle == TRUE & victims < 500)
df_clean_handgun <- subset(df_clean, assault_rifle == FALSE & handgun == TRUE)
df_positive_gun_type <- bind_rows(df_clean_assault, df_clean_handgun) %>% distinct()

# t-test showing more victims, more killed with assault rifles
t.test(df_clean_assault$victims, df_clean_handgun$victims)
t.test(df_clean_assault$killed, df_clean_handgun$killed)
t.test(df_clean_assault$injured, df_clean_handgun$injured)

sum(!is.na(import$assault_weapon))
sum(!is.na(import$rifle))
sum(!is.na(import$assault_weapon) | !is.na(import$rifle))

sum(!is.na(import$handgun_general))
sum(!is.na(import$handgun_specific))
sum(!is.na(import$handgun_general) | !is.na(import$handgun_specific))

congress_summary <- df_clean %>%
        group_by(state, congresional_district) %>%
        summarise(incidents = n(),
                  victims = sum(victims))

state_summary <- df_clean %>%
        group_by(state, assault_rifle) %>%
        summarise(incidents = n(),
                  victims = sum(victims))

state_summary_pos_gun_type <- df_positive_gun_type %>%
        group_by(state, assault_rifle) %>%
        summarise(incidents = n(),
                  victims = sum(victims))

incident_summary <- df_clean %>%
        group_by(date) %>%
        summarise(incidents = n(),
                  victims = sum(victims))
# Jeffrey B. Lewis, Brandon DeVine, Lincoln Pitcher, and Kenneth C. Martis. (2013)
# Digital Boundary Definitions of United States Congressional Districts, 1789-2012. 
# districts114.zip. Retrieved from http://cdmaps.polisci.ucla.edu on March 30, 2018.
# http://cdmaps.polisci.ucla.edu/tut/mapping_congress_in_R.html