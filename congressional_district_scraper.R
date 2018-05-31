library(tidyverse)
library(lubridate)
library(rvest)
library(rebus)


BASE_URL <- "https://www.house.gov/representatives"

page <- read_html(BASE_URL, encoding = "utf-8")
table <- html_table(page)
nodes <- html_nodes(page, "#by-name tr:nth-child(n)")
temp_nodes <- html_text(nodes)
contents <- strsplit(temp_nodes, "\n")
contents <- do.call(rbind, contents)
contents <- trimws(contents)
df <- as_tibble(contents)
df <- distinct(df)
names(df) <- contents[1,]
df <- df[-1,-7]

split_pattern <- SPACE %R% lookahead(DGT)
df <- df %>% separate(District, c("State", "District"), split_pattern)

