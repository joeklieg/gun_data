# webscraper for http://www.gunviolencearchive.org/
# pulls data for for mass shooting incidents in 2014-2018

library(tidyverse)
library(lubridate)
library(rvest)
library(rebus)


BASE_URLS <- paste0("http://www.gunviolencearchive.org/reports/mass-shooting?year=", 2014:2018, "&page=")
incident_url <- "http://www.gunviolencearchive.org"
pages <- 0:15 #enter the page ranges served by GVA for that year

df <- data.frame()
path <- list()

for (year in 2014:2018) {
     df[year,] <-
             map_df(pages, function(i) {
                     cat(".")
                     path[year,] <- paste0("http://www.gunviolencearchive.org/reports/mass-shooting?year=", year, "&page=", i)
                     page <- read_html(path[year,], encoding = "utf-8")
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
}
        
for (j in seq_along(BASE_URLS))
        BASE_URL[j] <- BASE_URLS[j]
        
}

for (j in seq_along(BASE_URLS)) {
        BASE_URL[j] <- BASE_URLS[j]
        df <-
                map_df(pages, function(i) {
                        cat(".")
                        path <- paste0(BASE_URL[j], i)
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
}


BASE_URL_2013 <- "http://www.gunviolencearchive.org/reports/mass-shootings/2012?page="



df <-
        map2_df(pages, BASE_URLS, function(i) {
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
df$date <- mdy(df$date)

# manual files from interactive session
write_csv(df, "mass_shootings_2013.csv")

# regex extractors
geolocation_regex <- DGT %R% DGT %R% DOT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% ", " %R% optional("-") %R%
        DGT %R% DGT %R% optional(DGT) %R% DOT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT)
congressional_district_regex <- "Congressional District: " %R% DGT %R% optional(DGT)
assault_weapon_regex <- "Assault weapon"
rifle_type_regex <- or("AR-15", "AK-47")
other_rifle_regex <- or("22 LR", "300 Win", "30-30 Win", "30-06 Spr", "308 Win")
shotgun_regex <- or("12 gauge", "16 gauge", "20 gauge", "28 gauge", "410 gauge")
handgun_regex <- or("H", "h") %R% "andgun"
handgun_type_regex <- or(DGT %R% DGT %R% optional(DGT) %R% SPACE %R% "Auto",
                         DGT %R% DGT %R% SPACE %R% "SW",
                         DGT %R% DGT %R% SPACE %R% "LR",
                         DGT %R% DGT %R% SPACE %R% "Mag",
                         DGT %R% DGT %R% SPACE %R% "Spl",
                         DGT %R% optional(DGT) %R% "mm")

# necessary to exlude las vegas because indident report is pdf not html
# also necessary because it is a statistical outlier


incident_pages <- df$incident

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


