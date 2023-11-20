# A Total Beginners Guide to Web Scraping Football Data 
#-- Part 1+2: https://sportsdatachallenge.wordpress.com/2016/09/21/a-total-beginners-guide-to-web-scraping-football-data-part-1/
#-- https://sportsdatachallenge.wordpress.com/2016/09/22/a-total-beginners-guide-to-web-scraping-football-data-part-2/

library(rvest)
library(tidyverse)

URL <- "https://www.betexplorer.com/soccer/team/beitar-jerusalem/t2BgeFT0/results"
WS <- read_html(URL)

## Get Table
# table <- WS %>%
#   html_nodes(xpath='/html/body/div[3]/div[4]/div/div/div[1]/section/div[2]/div/table') %>%
#   html_table()
# beitar <- table[[1]]

#-- Get all the clubs matches urls to scrape 
URLs <- WS %>%
  html_nodes(".h-text-center+ td a") %>%
  html_attr("href") %>%
  as.character()


#-- Links are missing the domain so we'll add them
URLs <- paste0("https://www.betexplorer.com/",URLs)

#--LOOOOPSSSSSSS

#-- Setup the Empty Data Frame to Store Data
Catcher1 <- data.frame(Home=character(),Away=character(),Date=character(),Score=character(), Timings = character(), Competition = character() )

#We want our code to go through each webpage and extract the URL for each players overview page.

for (i in URLs) {  #for each item, or i, in our list called URLs
  #Everything between the { and the } will be run for each item within our list URLs.
  
  WS1 <- read_html(i) #assign WS1 with the information from each webpage. 
  #The URL changes each time R loops through the code as we allocate ?i? into the () of read_html.
  
  Home <- WS1 %>%
    html_nodes(".list-details__item:nth-child(1) .list-details__item__title") %>%
    html_text() %>%
    as.character()
  
  
  Away <- WS1 %>%
    html_nodes(".list-details__item+ .list-details__item .list-details__item__title") %>%
    html_text() %>%
    as.character()
  
  Date <- WS1 %>%
    html_nodes("#match-date") %>%
    html_text() %>%
    as.character()
  
  Score <- WS1 %>%
    html_nodes("#js-score") %>%
    html_text() %>%
    as.character()
  
   Timings  <- WS1 %>%
    html_nodes(".list-details--shooters") %>%
    html_text() %>%
    as.character()
  
  Competition <- WS1 %>%
    html_nodes(".wrap-section__header__title a") %>%
    html_text() %>%
    as.character()
 
  if(length(Timings)>0){
    temp <- data.frame(Home,Away,Date,Score,Timings,Competition)
    Catcher1 <- rbind(Catcher1,temp)}
  
  else {
    Timings <- NA
    temp <- data.frame(Home,Away,Date,Score,Timings,Competition)
    Catcher1 <- rbind(Catcher1,temp)}
    

 # temp <- data.frame(Home,Away,Date,Score,Timings,Competition)
 # Catcher1 <- rbind(Catcher1,temp)
 #Waiting Time indication that the loop is runing
  cat("*")
}



#-- Data Wrangling----
final <- Catcher1

sapply(final, class)
columns <- final %>% ncol()
cols.num <- seq(from = 1, to = columns)
final[cols.num] <- sapply(final[cols.num],as.character)
sapply(final, class)


final <- final %>% 
        separate(Score, into = c("home_score", "away_score"), sep = ":")


final2 <- final %>%
  mutate(beitar_status = if_else(Home=="Beitar Jerusalem", "Home", "Away"))

final3 <- final2 %>%
  mutate(beitar_score = if_else(beitar_status=="Home", home_score, away_score)) %>%
  mutate(opponent_score =if_else(beitar_status=="Home", away_score, home_score))
  

final4 <- final3 %>%
        mutate(outcome=if_else(beitar_score > opponent_score, "Win", if_else(beitar_score == opponent_score, "Draw", "Lose")))


final5 <- final4 %>%
  mutate(game_id = row_number())



final6 <- final5 %>%
  mutate(nums = str_extract_all(Timings, "\\d+")) %>% 
  unnest(nums) %>%
  group_by(game_id) %>%
  mutate(goal = row_number()) %>%
  spread(goal, value = nums, sep = "")



final7 <- final6 %>% mutate(opponent=if_else(beitar_status=="Home", Away, Home))


## STOOPED HERE ###############
## RIDDLE ###########################################
## How to know which goals belongs to which team ?????? ###
############################################################
final8 <- final7 %>% select(outcome, Competition, Date,home_score, away_score, beitar_status, opponent, beitar_score, )



