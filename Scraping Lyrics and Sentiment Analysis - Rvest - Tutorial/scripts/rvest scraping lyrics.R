#-- http://stat545.com/111Scraping_Workthrough.html

library(tidyverse)
library(magrittr)
library(purrr)
library(glue)
library(stringr)
library(rvest)
library(xml2)

#-- Scrape lyrics from https://www.musixmatch.com/
#-- Get 
url_titles <- "https://www.musixmatch.com/artist/Straight-Line-Stitch"
page_title <- read_html(url_titles)
page_title %>% html_structure()

titles <- page_title %>%
         html_nodes(".title") %>%
         html_text()

SLS_df <- tibble(Band = "Straight Line Stitch",
                     Title = titles)


page_titles <- page_title %>%
  html_nodes(".title") %>%
  html_attr("href")


SLS_df %<>%
  mutate(Link = page_titles)

#-- Using Purrr
#--grab all lyrics. Let’s start with one at a time. What is the url we want?

url_song <- glue("https://www.musixmatch.com{SLS_df$Link[2]}")

#-- grab the lyrics for that song (url_song)

url_song %>%
  read_html() %>%
  html_nodes(".mxm-lyrics__content") %>%
  html_text()


#notice that it comes in different blocks: one for each section of text, broken by the advertisment.
#we can just collapse() them together with glue. 
#let’s turn that flow into a function:

get_lyrics <- function(link){
  
  lyrics_chunks <- glue("https://www.musixmatch.com{link}#") %>%
    read_html() %>%
    html_nodes(".mxm-lyrics__content")
  
  # we do a sanity check to see that there's something inside the lyrics!
  stopifnot(length(lyrics_chunks) > 0)
  
    lyrics <- lyrics_chunks %>%
    html_text() %>%
    glue_collapse(sep = "\n")
  
  return(lyrics)
}


#-- Example for the function
SLS_df$Link[3] %>%
  get_lyrics() %>%
  glue() # we paste into glue to get the nice formatting

## AKA get_lyrics(SLS_df$Link[3])


#-- purrr to map that function over our dataframe! --#
# we added possibly to skip missing lyrics for songs
get_lyrics_safe <- purrr::possibly(get_lyrics,NA_character_)

SLS_df %<>%
  mutate(Lyrics = map_chr(Link, get_lyrics_safe))




#### ANOTHER ARTIST ####
AH_url <- "https://www.musixmatch.com/artist/Angel-Haze"

AH_lyrics <- data_frame(Band = "Angel Haze",
                        
                        Title = AH_url %>%
                          read_html() %>%
                          html_nodes(css = ".title") %>%
                          html_text(),
                        
                        Link = AH_url %>%
                          read_html() %>%
                          html_nodes(css = ".title") %>%
                          html_attr("href"),
                        
                        Lyrics = map_chr(Link,get_lyrics))


#Bonus: sentiment analysis
library(tidytext)
afinn <- get_sentiments("afinn")

#we breaks the lyrics into their words, remove the words that are considered not interesting (they are called “stop words”), stitch the dataframe to the scoress from afinn, and do the math for each song.
SLS_df %>%
  unnest_tokens(word, Lyrics) %>% #split words
  anti_join(stop_words, by = "word") %>% #remove dull words
  inner_join(afinn, by = "word") %>% #stitch scores
  group_by(Title) %>% #and for each song
  summarise(Length = n(), Score = sum(value)/Length) %>% #do the math
  arrange(-Score)

#what was the most positive song?
  
  SLS_df %>%
  filter(Title == "Promise Me") %$%
  Lyrics %>%
  glue()

#same with Angela Haze:
    
AH_lyrics %>%
unnest_tokens(word, Lyrics) %>% #split words
anti_join(stop_words, by = "word") %>% #remove dull words
inner_join(afinn, by = "word") %>% #stitch scores
group_by(Title) %>% #and for each song
summarise(Length = n(), #do the math
Score = sum(score)/Length) %>%
arrange(-Score)
    
#turn some of those scripts into functions
get_words <- function(band_name){
      
# remove white space from band name
collapsed_name <- str_replace_all(band_name, " ", "-") 
      
      # define url to get the title and links
url <- glue("https://www.musixmatch.com/artist/{collapsed_name}")
      
      # read title page and extract the title chunks 
      title_page <- url %>%
        read_html() %>%
        html_nodes(css = ".title")
      
# and build the dataframe
lyrics <- data_frame(Band = band_name,
                           # extract text title
                           Title = title_page %>%
                             html_text(),
                           # extract title link
                           Link =  title_page %>%
                             html_attr("href"),
                           # map to get lyrics
                           Lyrics = map_chr(Link,get_lyrics))
      
      return(lyrics)
    }
    
      
#sentiment analysis
get_soul <- function(Lyrics_df) {
  Lyrics_df %>%
    unnest_tokens(word, Lyrics) %>% #split words
    #anti_join(stop_words, by = "word") %>% #remove dull words
    inner_join(afinn, by = "word") %>% #stitch scores
    group_by(Title) %>% #and for each song
    summarise(Length = n(), #do the math
              Score = sum(score)/Length) %>%
    arrange(-Score) %>%
    return()
}

# lets test
Billie_words <- "Billie Holiday" %>% get_words()
Billie_sentiment <- Billie_words %>% get_soul()


#### What if some songs dont have lyrics ####

ATCR_words <- "A Tribe Called Red" %>% get_words()

#either write ad hoc if ...else ... statements, to control for the presence/absence of things, or (and it is better to do it anyhow) wrap our function into purrr::possibly()

#now we use get_lyrics_safe() inside the mapping instead of get_lyrics().

get_lyrics_safe <- purrr::possibly(get_lyrics,NA_character_)

get_words <- function(band_name){
  
  # remove white space from band name
  collapsed_name <- str_replace_all(band_name, " ", "-") 
  
  # define url to get the title and links
  url <- glue("https://www.musixmatch.com/artist/{collapsed_name}")
  
  # read title page and extract the title chunks 
  title_page <- url %>%
    read_html() %>%
    html_nodes(css = ".title")
  
  # and build the dataframe
  lyrics <- data_frame(Band = band_name,
                       # extract text title
                       Title = title_page %>%
                         html_text(),
                       # extract title link
                       Link =  title_page %>%
                         html_attr("href"),
                       # map to get lyrics
                       Lyrics = map_chr(Link,get_lyrics_safe))
  
  return(lyrics)
}

#Let’s try again:
ATCR_words <- "A Tribe Called Red" %>% get_words()
ATCR_sentiment <- ATCR_words %>% get_soul()



