library(tidyverse)

import <- read_csv("df_scrape_incl_vegas.csv")

congress_clean <- DGT %R% optional(DGT)

import$congresional_district <- str_extract(test, congress_clean)
import$assault_rifle <- (!is.na(import$assault_weapon) | !is.na(import$rifle))
import$handgun <- (!is.na(import$handgun_general) | !is.na(import$handgun_specific))


df_clean <- import %>%
        select(date:city, geolocation:congresional_district, assault_rifle:handgun)


df_clean_assault <- subset(df_clean, assault_rifle == TRUE & victims < 500)
df_clean_handgun <- subset(df_clean, assault_rifle == FALSE & handgun == TRUE)

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
