# webscraper for http://www.gunviolencearchive.org/
# pulls data for for mass shooting incidents in 2013-2018

library(tidyverse)
library(lubridate)
library(rvest)
library(rebus)


BASE_URLS <- paste0("http://www.gunviolencearchive.org/reports/mass-shooting?year=", 2014:2018, "&page=")
BASE_URLS_1 <- paste0("http://www.gunviolencearchive.org/reports/mass-shootings/2013?page=")
incident_url <- "http://www.gunviolencearchive.org"
pages <- 0:15 #enter the page ranges served by GVA for that year


df_2013 <-  map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URLS_1, i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_paste <- paste0(incident_url, incident_extract)
        table <- html_table(page)
        
        data.frame(table, incident_paste, stringsAsFactors=FALSE)
        
})

df_2014 <-  map_df(pages, function(i) {
                cat(".")
                path <- paste0(BASE_URLS[1], i)
                page <- read_html(path, encoding = "utf-8")
                incident <- html_nodes(page, "tr:nth-child(n)")
                incident_regex <- "/" %R% "incident" %R% "/" %R%
                        DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
                incident_extract <- na.omit(str_extract(incident, incident_regex))
                incident_paste <- paste0(incident_url, incident_extract)
                table <- html_table(page)
                        
                data.frame(table, incident_paste, stringsAsFactors=FALSE)
                        
                })

df_2015 <-  map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URLS[2], i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_paste <- paste0(incident_url, incident_extract)
        table <- html_table(page)
        
        data.frame(table, incident_paste, stringsAsFactors=FALSE)
        
})

df_2016 <-  map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URLS[3], i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_paste <- paste0(incident_url, incident_extract)
        table <- html_table(page)
        
        data.frame(table, incident_paste, stringsAsFactors=FALSE)
        
})

df_2017 <-  map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URLS[4], i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_paste <- paste0(incident_url, incident_extract)
        table <- html_table(page)
        
        data.frame(table, incident_paste, stringsAsFactors=FALSE)
        
})

df_2018 <-  map_df(pages, function(i) {
        cat(".")
        path <- paste0(BASE_URLS[5], i)
        page <- read_html(path, encoding = "utf-8")
        incident <- html_nodes(page, "tr:nth-child(n)")
        incident_regex <- "/" %R% "incident" %R% "/" %R%
                DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT)
        incident_extract <- na.omit(str_extract(incident, incident_regex))
        incident_paste <- paste0(incident_url, incident_extract)
        table <- html_table(page)
        
        data.frame(table, incident_paste, stringsAsFactors=FALSE)
        
})

df_clean <- function(df) {
        names(df) <- c("date", "state", "city", "address", "killed", "injured", "details", "incident")
        df <- select(df, date, state, city, killed, injured, incident)
        df$date <- mdy(df$date)
        df
}

df_2013 <- df_clean(df_2013)
df_2014 <- df_clean(df_2014)
df_2015 <- df_clean(df_2015)
df_2016 <- df_clean(df_2016)
df_2017 <- df_clean(df_2017)
df_2018 <- df_clean(df_2018)

df_clean <- bind_rows(df_2013, df_2014, df_2015, df_2016, df_2017, df_2018) %>%
        mutate(victims = killed + injured) %>%
        filter(victims < 500) %>%
        distinct()

# manual files from interactive session
write_csv(df_clean, "data/df_clean.csv")

