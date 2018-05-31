library(tidyverse)
library(mapdata)

import <- read_csv("data/df_clean_scrape.csv")
congress_clean <- DGT %R% optional(DGT)
import$congresional_district <- str_extract(import$congresional_district, congress_clean)

import$assault_rifle <- (!is.na(import$rifle))
import$handgun <- (!is.na(import$handgun_general) | !is.na(import$handgun_specific))

df_clean_assault <- subset(import, assault_rifle == TRUE)
df_clean_assault$gun_type <- "assault_rifle"
df_clean_handgun <- subset(import, assault_rifle == FALSE & handgun == TRUE)
df_clean_handgun$gun_type <- "handgun"
df_positive_gun_type <- bind_rows(df_clean_assault, df_clean_handgun) %>%
        select(-incident, -(assault_weapon:handgun))

ggplot(df_positive_gun_type, aes(gun_type, victims)) +
        geom_boxplot(alpha = 0.2, outlier.colour = NA) +
        geom_jitter(aes(color = gun_type)) +
        

ggplot(df_positive_gun_type, aes(gun_type, injured)) + geom_boxplot()
ggplot(df_positive_gun_type, aes(gun_type, killed)) + geom_boxplot()


# t-test showing more victims, more killed with assault rifles
t.test(df_clean_assault$victims, df_clean_handgun$victims)
t.test(df_clean_assault$killed, df_clean_handgun$killed)
t.test(df_clean_assault$injured, df_clean_handgun$injured)




congress_summary_assault <- df_clean_assault %>%
        group_by(state, congresional_district) %>%
        summarise(incidents = n(),
                  victims = sum(victims))

congress_summary_handgun <- df_clean_handgun %>%
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