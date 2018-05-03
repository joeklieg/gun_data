library(purrr)
library(rvest)
library(xml2)
library(rebus)

import <- read_csv("df_scrape_excl_vegas.csv")
# extractors
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

# gun types: AR-15, AK-47, 25 Auto, 40 SW, 9mm, 45 SW,
#necessary to exlude las vegas because indident report is pdf not html

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

rifle_type <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                rifle_type <- str_extract(gun, rifle_type_regex)
                data.frame(rifle_type, stringsAsFactors = FALSE)
        })

other_rifle_type <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                other_rifle_type <- str_extract(gun, other_rifle_regex)
                data.frame(other_rifle_type, stringsAsFactors = FALSE)
        })

shotgun_type <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                shotgun_type <- str_extract(gun, shotgun_regex)
                data.frame(shotgun_type, stringsAsFactors = FALSE)
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

handgun_type <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                handgun_type <- str_extract(gun, handgun_type_regex)
                data.frame(handgun_type, stringsAsFactors = FALSE)
        })

congressional <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                congressional_district <- str_extract(gun, congressional_district_regex)
                data.frame(congressional_district, stringsAsFactors = FALSE)
        })

df_add <- bind_cols(df, geolocation, congressional, rifle_type, handgun, handgun_type, shotgun_type, other_rifle_type)
df_scrape <- select(df_add, -c(address, assault_weapon, rifle, shotgun_type, other_rifle_type))
df_scrape <- select(df_scrape, date:congresional_district, rifle_type, everything())

df_vegas$geolocation <- "36.0919,-115.1751"
df_vegas$congresional_district <- "Congressional District: 1"
df_vegas$assault_weapon <- "Assault weapon"
df_vegas$rifle <- "AR-15"
df_vegas$handgun_general <- "Handgun"
df_vegas$handgun_specific <- "9mm"

df_scrape <- df_scrape %>% rename(congressional_district = congresional_district)
congress_clean <- DGT %R% optional(DGT)

df_scrape$congressional_district <- str_extract(df_scrape$congressional_district, congress_clean)


write_csv(df_add, "df_scrape_2013.csv")
write_csv(df_scrape, "df_scrape_excl_vegas_v2.csv")
