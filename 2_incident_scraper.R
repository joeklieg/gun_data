# regex extractors
geolocation_regex <- DGT %R% DGT %R% DOT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT) %R% "," %R% optional(SPACE) %R% optional("-") %R%
        DGT %R% DGT %R% optional(DGT) %R% DOT %R% DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT)
congressional_district_regex <- "Congressional District: " %R% DGT %R% optional(DGT)
rifle_type_regex <- or("AR-15", "AK-47")
other_rifle_regex <- or("22" %R% optional(SPACE) %R% "LR",
                        "300" %R% optional(SPACE) %R% "Win",
                        "30-30" %R% optional(SPACE) %R% "Win",
                        "30-06" %R% optional(SPACE) %R% "Spr",
                        "308" %R% optional(SPACE) %R% "Win")
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


incident_pages <- df_clean$incident

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
                rifle <- str_extract(gun, rifle_type_regex)
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

handgun_type <-
        map_df(incident_pages, function(i) {
                cat(".")
                page <- read_html(i, encoding = "utf-8")
                gun_node <- html_node(page, "#block-system-main")
                gun <- html_text(gun_node)
                handgun_specific <- str_extract(gun, handgun_type_regex)
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

df_clean_scrape <- bind_cols(df_clean, geolocation, congressional, rifle_type, handgun, handgun_type)

write_csv(df_clean_scrape, "data/df_clean_scrape.csv")


