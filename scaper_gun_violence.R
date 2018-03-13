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

incident_pages <- df$incident


# for some years the pull doesn't work, still figuring out why
df_gun <-
        map_df(incident_pages, function(i) {
        cat(".")
        page <- read_html(i, encoding = "utf-8")
        gun_node <- html_node(page, "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
        gun_node2 <- html_node(page, "#block-system-main > div:nth-child(5) > ul > li:nth-child(1)")
        gun <- html_text(gun_node)
        gun2 <- html_text(gun_node2)

        data.frame(gun, gun2, stringsAsFactors = FALSE)
  })

df_gun[is.na(df_gun)] <- ""
df_gun[df_gun == "\n "] <- ""
df_gun$clean <- paste0(df_gun$gun, df_gun$gun2)
df_gun$clean <- str_replace(df_gun$clean, "Type: ", "")
df_clean <- tibble(gun = df_gun$clean)
df_total <- bind_cols(df, df_clean)

write_csv(df, "mass_shootings_[enter_year].csv")
