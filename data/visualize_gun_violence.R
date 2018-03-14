
library(tidyverse)
library(stringr)
library(lubridate)
library(rebus)

#fl <- list.files(".", "mass*"
#df <- list()
#for (f in fl) {
#        df[[f]] <- read_csv(f)
#}
d1 <- read_csv("~/Documents/GitHub/gun_data/data/mass_shootings_2014.csv")
d2 <- read_csv("~/Documents/GitHub/gun_data/data/mass_shootings_2015.csv")
d3 <- read_csv("~/Documents/GitHub/gun_data/data/mass_shootings_2016.csv")
d4 <- read_csv("~/Documents/GitHub/gun_data/data/mass_shootings_2017.csv")
d5 <- read_csv("~/Documents/GitHub/gun_data/data/mass_shootings_2018.csv")

df_f1 <- bind_rows(d1, d2, d3, d4)
df_f2 <- d5 

df_f1$date <- parse_date_time(df_f1$date, "m d, y")
df_f2$date <- parse_date_time(df_f2$date, "d-m-y")
df <- bind_rows(df_f1, df_f2) %>%
        distinct() %>%
        mutate(victims = killed + injured)

spread(df, date, c("year", "month", "day"), sep = "-")
?spread
df_date <- df %>%
        group_by(month(date), victims) %>%
        count()

ggplot(df, aes(date, victims)) + geom_point()

