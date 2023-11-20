#--A Crazy Little Thing Called {purrr} - Part 1 : Web Mining
#rvest the data
library(rvest)
library(tidyverse)
get_album_list <- function(url){
  read_html(url)  %>% 
    html_nodes(".col-md-12") %>%
    html_nodes("a") %>%
    html_attr("href")
  
}

url_album <- get_album_list("http://paroles2chansons.lemonde.fr/paroles-michel-sardou/discographie.html")

get_album_info <- function(url){
  page <- read_html(url) 
  date <- page %>% 
    html_nodes("small") %>%
    html_text() %>%
    stringr::str_replace_all("Date de Sortie : ", "") %>%
    lubridate::dmy()
  song_list <- page %>% 
    html_nodes(".font-small") %>%
    html_text() %>%
    discard(~ .x == "Plan de site" | .x == "Mention légale" | .x == "Chansons de mariage" | .x == "Chansons d'enterrement" )
  
  url_list <- page %>% 
    html_nodes(".font-small") %>%
    html_attr("href") %>%
    discard(~ .x == "/plan-du-site.html" | .x == "/mentions-legales.html" | .x == "/paroles-chansons-de-messe-d-enterrement/"| .x == "/paroles-chansons-de-messe-de-mariage/")
  
  album_name <- page %>%
    html_nodes(".breadcrumb") %>%
    html_text() %>%
    stringr::str_extract("\t.*$") %>%
    stringr::str_replace_all("\t", "")
  
  tibble(chanson = song_list, 
         url = url_list, 
         nom = album_name, 
         date = date)
  
}

albums_infos <- map_df(url_album, get_album_info) %>%
  filter(grepl("sardou", url))

get_lyrics <- function(url, name){
  page <- read_html(url)
  lyrics <- page %>%
    html_nodes(".text-center") %>%
    html_nodes("div") %>%
    html_text() %>%
    stringr::str_replace_all("[\t+\r+\n+]", " ") %>%
    stringr::str_replace_all("[ ]{2}", " ") %>%
    stringr::str_replace_all("googletag.cmd.push\\(function\\(\\) \\{ googletag.display\\('container-middle-lyrics'\\)\\; \\}\\)\\;", "") %>% 
    stringr::str_replace_all("\\/\\* ringtone - Below Lyrics \\*\\/.*", "") %>%
    discard( ~ grepl("Corriger les paroles", .x)) %>%
    discard( ~ grepl("Paroles2Chansons", .x)) %>%
    discard( ~ nchar(.x)  < 2) 
  tibble(parole = lyrics, 
         song = name)
}
safe_lyr <- safely(get_lyrics)

lyrics_df <- map2(albums_infos$url, 
                  albums_infos$chanson, 
                  ~ safe_lyr(.x,.y)) %>%
  map("result") %>%
  compact() %>%
  reduce(bind_rows) %>%
  filter(! grepl("Soumettre une chanson", parole) )

albums_infos <- albums_infos %>%
  left_join(lyrics_df, by = c(chanson = "song")) 


