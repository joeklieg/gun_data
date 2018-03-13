# webscraper for http://www.gunviolencearchive.org/
# increase the page range for more data

library(tidyverse)
library(rvest)
library(rebus)

BASE_URL <- "http://www.gunviolencearchive.org/reports/mass-shooting?page="
incident_url <- "http://www.gunviolencearchive.org"
pages <- 0

df <-
  map_df(pages, function(i) {
    # keep calm!
    cat(".")
    path <- paste0(BASE_URL, i)
    page <- read_html(path, encoding = "utf-8")
    incident <- html_nodes(page, ":nth-child(1) a")
    incident_regex <- "/" %R% "incident" %R% "/" %R%
      DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT
    incident_extract <- na.omit(str_extract(incident, incident_regex))
    incident_extract <- paste0(incident_url, incident_extract)
    table <- html_table(page)

    data.frame(table,
               incident_extract,
               stringsAsFactors=FALSE)
    
  })
names(df) <- c("date", "state", "city", "address", "killed", "injured", "details", "incident")


incident_pages <- paste0(incident_url, df$incident)

df_gun <-
  map_df(incident_pages, function(i) {
    page[i] <- read_html(incident_pages[i], encoding = "utf-8")
    gun_node[i] <- html_node(page[i], "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
    gun[i] <- html_text(gun_node[i])
  })


write_csv(df, "data/output.csv")


page <- read_html(incident_pages[1], encoding = "utf-8")
gun_node <- html_node(page, "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
gun <- html_text(gun_node)

for (i in seq_along(check))  {
  gun_node <- html_node(test[1], "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
}
gun_node
gun_node <- html_node(test[[1]], "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
test[1]
t_page <- read_html("http://www.gunviolencearchive.org/incident/1067296", encoding = "utf-8")
t_gun <- html_node(t_page, "#block-system-main > div:nth-child(6) > ul > li:nth-child(1)")
t_gun
html_text(t_gun)

write_lines(t_table, "~/Downloads/t_table.csv")
