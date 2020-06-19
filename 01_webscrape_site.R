################################################################################
# Scraping the UKSA website. This script creates theme_tbl and letters_tbl
################################################################################

# packages ----------------------------------------------------------------
library(rvest) # for getting html data
library(tidyverse) # for data wrangling 
library(lubridate) # for date wrangling


# scrape set up ------------------------------------------------------------------

base_url <- "https://www.statisticsauthority.gov.uk/correspondence-list/"
# read in the data
uksa_homepage <- read_html(base_url)

# correspondence by theme -------------------------------------------------

# get the filter options from the selector on the left of the webpage
all_filter_options <- html_nodes(uksa_homepage,css = '.form-control')
# manual step that the theme dropdown is the fourth in the list
theme_options <- all_filter_options[[4]]
# go a level deeper
themes <- html_children(theme_options)
# now we create a table that can be thought of as "human readable name" and 
# "machine readable name". Really neat
theme_tbl <- tibble(names = html_text(themes),
                    values = unlist(html_attrs(themes)))

# make a function that for a given webpage finds the OSR number letters found
find_number_correspondences <- function(url){
 url %>% 
    read_html() %>%
    # found the node was ".lead" using chrome developer tools
    html_nodes('.lead') %>%
    html_text() %>%
    # hopefully we'll just be left with a number
    stringr::str_replace(" item\\(s\\) of correspondence found", "") %>%
    as.numeric()
  }

# show it in action for homepage
find_number_correspondences(base_url) # 1243 at 2020-06-19

# now apply to every theme
theme_tbl <- theme_tbl %>%
  # the urls are consistent (machine readable bit)
  mutate(url = paste0(base_url,"?keyword=&theme=", values)) %>%
  rowwise() %>%
  mutate(num_letters = find_number_correspondences(url)) %>%
  ungroup()

# individual correspondences ----------------------------------------------

# get all those juicy correspondences - very nice they have a "show all" button
# so we don't have to cycle through urls :) 
motherload_url <- paste0(base_url,"/?keyword=&theme=&date=&show=all")
motherload_page <- read_html(motherload_url, encoding ="UTF-8" )

# nested down until we find the information we're after
text <- motherload_page %>%
  html_node(".listing") %>% 
  html_children() %>%
  html_children() %>% 
  html_children() %>%
  html_text()
length(text)/5 #the number of unique correspondences
text[1:5] # the information from the most recent correspondence

# a big ol' tibble where one row is one correspondence (nice and tidy)
# A bit hacky in that we've made assumption each item has 5 levels, but
# it fits and looks right on inspection
letters_tbl <- tibble(
  title    = text[seq(2, length(text), 5)],
  date     = text[seq(3, length(text), 5)] %>% lubridate::dmy(),
  subtitle = text[seq(4, length(text), 5)]
)

# lets find the URL for each correspondence
letters_url <- motherload_page %>% 
  html_node(".listing") %>%
  html_children %>% 
  # the first hyperlink of each item is the page url
  html_node("a:first-of-type") %>%
  # get the hyperlink
  html_attr("href")
# add it to our tbl. Again assumption of order of letters, but it's fine
letters_tbl <- letters_tbl %>% 
  mutate(url = letters_url)
