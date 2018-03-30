# webscraper for http://www.gunviolencearchive.org/
# pulls data for for mass shooting incidents in 2014-2018

library(tidyverse)
library(rvest)
library(rebus)

BASE_URL <- "http://www.gunviolencearchive.org/reports/mass-shooting?year=[enter_year]&page="
incident_url <- "http://www.gunviolencearchive.org"
pages <- 0:10 #enter the page ranges served by GVA for that year

df <-
        map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URL, i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_extract <- paste0(incident_url, incident_extract)
        table <- html_table(page)

        data.frame(table,
                incident_extract,
                stringsAsFactors=FALSE)

  })

names(df) <- c("date", "state", "city", "address", "killed", "injured", "details", "incident")
df <- select(df, date, state, city, address, killed, injured, incident)

# manual files from interactive session
write_csv(df, "mass_shootings_[enter_year].csv")

# regex extractors
geolocation_regex <- DGT %R% DGT %R% DOT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% ", " %R% optional("-") %R%
        DGT %R% DGT %R% optional(DGT) %R% DOT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT)
congressional_district_regex <- "Congressional District: " %R% DGT %R% optional(DGT)
assault_weapon_regex <- "Assault weapon"
rifle_ar15_regex <- "AR-15"
handgun_regex <- or("H", "h") %R% "andgun"
handgun_9mm_regex <- "9mm"

# necessary to exlude las vegas because indident report is pdf not html
# also necessary because it is a statistical outlier
df_exlude_vegas <- subset(df, victims < 500)
df_vegas <- subset(df, victims >= 500)

incident_pages <- df_exlude_vegas$incident

geolocation <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                geolocation <- str_extract(gun, geolocation_regex)
                data.frame(geolocation, stringsAsFactors = FALSE)
        })


assault <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                assault_weapon <- str_extract(gun, assault_weapon_regex)
                data.frame(assault_weapon, stringsAsFactors = FALSE)
        })

rifle_ar15 <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                rifle <- str_extract(gun, rifle_ar15_regex)
                data.frame(rifle, stringsAsFactors = FALSE)
        })

handgun <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                handgun_general <- str_extract(gun, handgun_regex)
                data.frame(handgun_general, stringsAsFactors = FALSE)
        })

handgun_9mm <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                handgun_specific <- str_extract(gun, handgun_9mm_regex)
                data.frame(handgun_specific, stringsAsFactors = FALSE)
        })

congressional <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                congresional_district <- str_extract(gun, congressional_district_regex)
                data.frame(congresional_district, stringsAsFactors = FALSE)
        })

df_add <- bind_cols(df_exlude_vegas, geolocation, congressional, assault, rifle_ar15, handgun, handgun_9mm)

# vegas details manual input
df_vegas$geolocation <- "36.0919,-115.1751"
df_vegas$congresional_district <- "Congressional District: 1"
df_vegas$assault_weapon <- "Assault weapon"
df_vegas$rifle <- "AR-15"
df_vegas$handgun_general <- "Handgun"
df_vegas$handgun_specific <- "9mm"

df_scrape <- bind_rows(df_add, df_vegas)

write_csv(df_add, "df_scrape_excl_vegas.csv")
write_csv(df_scrape, "df_scrape_incl_vegas.csv")


