
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

state_pop <- read_csv("~/Documents/GitHub/gun_data/data/PEP_2017_PEPANNRES/PEP_2017_PEPANNRES_with_ann.csv", skip = 1)
state_pop <- select(state_pop, Geography, `Population Estimate (as of July 1) - 2016`) %>%
                rename(state_population = `Population Estimate (as of July 1) - 2016`)
city_pop <- read_csv("~/Documents/GitHub/gun_data/data/PEP_2016_PEPANNRSIP.US12A/PEP_2016_PEPANNRSIP.US12A_with_ann.csv", skip = 1)
city_pop <- select(city_pop, Geography_2, `Population Estimate (as of July 1) - 2016`) %>%
                rename(state = Geography_2,
                       city_population = `Population Estimate (as of July 1) - 2016`)
city_pop <- str_split(city_pop$state, " city, ", 2, simplify = TRUE)

?str_split_fixed

area <- read_csv("~/Documents/GitHub/gun_data/data/state_area.csv")

state.name2 <- append(state.name, "District of Columbia")
state.region2 <- as.factor(append(state.region, "2"))
levels(state.region2) <- c("Northeast", "South", "North Central", "West")
state_stats <- bind_cols(state = state.name2, state_region = state.region2) %>%
        inner_join(area, by = "state")


df_f1 <- bind_rows(d1, d2, d3, d4)
df_f2 <- d5

df_f1$date <- parse_date_time(df_f1$date, "m d, y")
df_f2$date <- parse_date_time(df_f2$date, "d-m-y")
df <- bind_rows(df_f1, df_f2) %>%
        distinct() %>%
        mutate(victims = killed + injured) %>%
        inner_join(state_pop, by = c("state" = "Geography")) %>%
        inner_join(state_stats, by = "state")

# how many mass shootings per date, day, month, year
df_date <- df %>%
        group_by(date) %>%
        count()

df_day <- df %>%
        group_by(day(date)) %>%
        count()

df_month <- df %>%
        group_by(month(date)) %>%
        count()

df_year <- df %>%
        group_by(year(date)) %>%
        count()

# to quantify number of shootings by state
df_state <- df %>%
        group_by(state) %>%
        count() %>%
        inner_join(state_pop, by = c("state" = "Geography")) %>%
        left_join(state_stats, by = "state") %>%
        mutate(normalized_shooting_rate = n / state_population)

df_city <- df %>%
        group_by(city) %>%
        count() %>%
        inner_join(city_pop, by )
# to quantify the number of victims per shooting by state
vics_per_shooting <- df %>%
        group_by(state, victims) %>%
        count()
vics_per_shooting2 <- vics_per_shooting %>%
        mutate(total_victims = victims * n)
vics_per_shooting3 <- vics_per_shooting2 %>%
        group_by(state) %>%
        summarise_at(vars(total_victims, n), sum) %>%
        mutate(victims_per_shooting = total_victims / n)

# to quantify the number of mass shootings normalized by state population
df_region <- df %>%
        group_by(state_region, state_population) %>%
        count()
df_region <- df_region %>%
        group_by(state_region) %>%
        summarise_at(vars(state_population, n), sum) %>%
        mutate(normalized_shooting_rate = n / state_population)
