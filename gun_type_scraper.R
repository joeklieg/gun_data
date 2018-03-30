library(purrr)
library(rvest)
library(xml2)
library(rebus)

incident_pages <- df$incident[1:5]
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

test <- read_html("http://www.gunviolencearchive.org/incident/476321", package = "xml2") %>%
        xml_find_all("//div") %>%
        xml_attrs("block-system-main")
test

# //*[@id="block-system-main"]/div[7]/text()[1]
xml_text(test)

# geolocation extractor
gun_type_regex <- DGT %R% DGT %R% DOT %R% DGT %R% DGT %R% DGT %R% DGT %R% ", " %R% optional("-") %R%
                        DGT %R% DGT %R% DOT %R% DGT %R% DGT %R% DGT %R% DGT
str_extract(test , gun_type_regex)
gun_type_regex
?one_or_more
#Congressional district extractor
congressional_district_regex <- "Congressional District: " %R% DGT %R% optional(DGT)
str_view(test, congressional_district_regex)
Geolocation: 29.9304, -90.0959
Congressional District: 2
Guns Involved
Type: 223 Rem [AR-15]
Type: Handgun
?read_html

?xml_find_all
# Find all rev nodes anywhere in document
rev_nodes <- xml_find_all(rev_xml, "//rev")

# Use xml_text() to get text from rev_nodes
xml_text(rev_nodes)



###other stuff
resp_xml <- rev_history("Hadley Wickham", format = "xml")

# Check response is XML 
http_type(resp_xml)

# Examine returned text with content()
rev_text <- content(resp_xml, as = "text")
rev_text

# Turn rev_text into an XML document
rev_xml <- read_xml(rev_text)