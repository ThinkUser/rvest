#-- http://stat545.com/111Scraping_Workthrough.html

library(tidyverse)
library(magrittr)
library(purrr)
library(glue)
library(stringr)
library(rvest)
library(xml2)

#-- Scrape lyrics from https://www.musixmatch.com/

#-- Band Titles
url_titles <- "https://www.musixmatch.com/artist/Straight-Line-Stitch"
page_title <- read_html(url_titles)
page_title %>% html_structure()

titles <- page_title %>%
  html_nodes(".title") %>%
  html_text()

SLS_df <- tibble(Band = "Straight Line Stitch",
                 Title = titles)

#-- Title Links from Band Page
page_titles <- page_title %>%
  html_nodes(".title") %>%
  html_attr("href")

SLS_df %<>%
  mutate(Link = page_titles)

#-- Using loops
#--add domain to links

URLs <- paste0("https://www.musixmatch.com",SLS_df$Link)

#-- Create an empty df
Catcher1 <- data.frame(song_lyrics=character(), song_name=character())

#-- For loop to read all the links in the artist name with else fall backs
for(i in URLs){
  
  #-- Get Lyrics chuncks
  WS1 <- read_html(i)
  song_lyrics_chuncks <-  WS1 %>%
                  html_nodes(".mxm-lyrics__content") %>%
                  html_text()%>%
                  as.character()
  
  #-- Get song title
  song_name <- WS1 %>% 
               html_nodes(".mxm-track-title__track") %>%
               html_text()%>%
               as.character()
  
  if(length(song_lyrics_chuncks)>0){
    lyrics <- song_lyrics_chuncks %>%
      glue_collapse(sep = "\n")
    temp <- data.frame(lyrics, song_name)
        Catcher1 <- rbind(Catcher1,temp)}
    else {
      
      lyrics <- NA
      temp <- data.frame(lyrics, song_name)
      Catcher1 <- rbind(Catcher1,temp)}
     
 
  cat("*")
      
}


#-- Clean Data Frame Catcher 1
glimpse(Catcher1)
Catcher1$song_name <- as.character(Catcher1$song_name)
Catcher1$lyrics <- as.character(Catcher1$lyrics)
Catcher1$song_name <-  gsub("Lyrics","",Catcher1$song_name)
Catcher1$band_name <- SLS_df$Band[1]
glimpse(Catcher1)
Catcher1 <- Catcher1 %>% select(band_name, song_name, lyrics)

#Bonus: sentiment analysis
library(tidytext)
afinn <- get_sentiments("afinn")


#break the lyrics into their words
#remove the words that are considered not interesting (they are called “stop words”)
#stitch the dataframe to the scoress from afinn
#do the math for each song.

score_list <- Catcher1 %>% unnest_tokens(word, lyrics) %>% #split to words
          anti_join(stop_words, by = "word") %>% #remove dull words
          inner_join(afinn, by = "word") %>% #stitch scores
          group_by(song_name) %>% #and for each song
          summarise(Length = n(), Score = sum(value)/Length) %>% #do the math
          arrange(-Score)

#add score to a new dataframe
Catcher2 <- merge(Catcher1, score_list, by="song_name")

#what was the most positive song?
most_positive <- as.character(Catcher2 %>% filter(Score==max(Catcher2$Score)) %>% select(song_name))
most_negative <- as.character(Catcher2 %>% filter(Score==min(Catcher2$Score)) %>% select(song_name))



#### Words Functions ####
