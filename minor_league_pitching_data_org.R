#Scrape minor league pitching data
library(rvest)
library(tidyverse)

#Base url
url <- "https://www.baseball-reference.com/register/affiliate.cgi?year=2022"

#Acquire team names
tms <- url %>% xml2::read_html() %>% rvest::html_nodes("th a") %>% rvest::html_text(trim = T)

#Team name and url in team_urls
team_urls <- tibble(
  url = paste0("https://www.baseball-reference.com",url %>% xml2::read_html() %>% rvest::html_nodes("th a") %>% rvest::html_attr('href')),
  team = tms)

#Function
acquire_pitching_stats_bref <- function(bref_url, team) {
  
  df <- bref_url %>% xml2::read_html() %>%
    rvest::html_nodes(xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse='') %>%
    xml2::read_html() %>% 
    rvest::html_nodes("#aff_pitching") %>% 
    rvest::html_table(trim=T) %>% .[[1]] %>% tail(1) %>%
    mutate(Tm = team)
  
  #Account for errors you pesky website
  Sys.sleep(.001)
  
  return(df)
  
}  

#pull in data
ml_pitching <- 1:nrow(team_urls) %>% purrr::map(function(x) acquire_pitching_stats_bref(team_urls$url[x], team_urls$team[x]), .progress = T)
#Coerce to data frame
ml_pitching <- ml_pitching %>% bind_rows()