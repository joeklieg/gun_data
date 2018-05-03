library(tidyverse)
library(lubridate)

import_2018 <- read_csv("data/mass_shootings_2018.csv")
import_2017 <- read_csv("data/mass_shootings_2017.csv")
import_2016 <- read_csv("data/mass_shootings_2016.csv")
import_2015 <- read_csv("data/mass_shootings_2015.csv")
import_2014 <- read_csv("data/mass_shootings_2014.csv")
import_2013 <- read_csv("data/mass_shootings_2013.csv")

df <- bind_rows(import_2013, import_2014, import_2015, import_2016, import_2017, import_2018)
df$date <- mdy(df$date)
df <- df %>%
        bind_rows(import_2018) %>%
        mutate(victims = killed + injured) %>%
        select(date, killed, injured, victims, everything()) %>%
        distinct()

n_distinct(df)
n_distinct(df$date)
n_distinct(df$state)
n_distinct(df$city)
n_distinct(df$incident)
sum(df$killed)
sum(df$injured)

#summaries by incident
table(year(df$date))
table(month(df$date))
table(day(df$date))

#victims by year
victims_by_year <- df %>%
        group_by(year = year(date)) %>%
        summarise(number_incidents = n(),
                  number_killed = sum(killed),
                  number_injured = sum(injured),
                  number_victims = sum(victims)) %>%
        mutate(killed_per_incident = number_killed / number_incidents,
               injured_per_incident = number_injured / number_incidents,
               victims_per_incident = number_victims / number_incidents)

#victims by month
victims_by_month <- df %>%
        group_by(month = month(date)) %>%
        summarise(number_incidents = n(),
                  number_killed = sum(killed),
                  number_injured = sum(injured),
                  number_victims = sum(victims)) %>%
        mutate(killed_per_incident = number_killed / number_incidents,
               injured_per_incident = number_injured / number_incidents,
               victims_per_incident = number_victims / number_incidents)
       
#victims by state
victims_by_state <- df %>%
        group_by(state) %>%
        summarise(number_incidents = n(),
                  number_killed = sum(killed),
                  number_injured = sum(injured),
                  number_victims = sum(victims)) %>%
        mutate(killed_per_incident = number_killed / number_incidents,
               injured_per_incident = number_injured / number_incidents,
               victims_per_incident = number_victims / number_incidents)

#victims by city
victims_by_city <- df %>%
        group_by(city) %>%
        summarise(number_incidents = n(),
                  number_killed = sum(killed),
                  number_injured = sum(injured),
                  number_victims = sum(victims)) %>%
        mutate(killed_per_incident = number_killed / number_incidents,
               injured_per_incident = number_injured / number_incidents,
               victims_per_incident = number_victims / number_incidents)
