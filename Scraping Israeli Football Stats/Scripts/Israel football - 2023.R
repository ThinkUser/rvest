#-- Scraping Israeli Football --#

library(rvest)
library(tidyverse)
URL <- "https://www.football.co.il" # 
WS <- read_html(URL)

#-- Get all the clubs urls to scrape from the premier league page
URLs <- WS %>%
  html_nodes(".pointsTableTeamName>a") %>%
  html_attr("href") %>%
  as.character()


#-- Links are missing the domain so we'll add them
URLs <- paste0("https://www.football.co.il/",URLs)

#--LOOOOPSSSSSSS
#-- the information we will be extracting is the URL address of all players with each squad. 


#-- Setup the Empty Data Frame to Store Data
Catcher1 <- data.frame(Player=character(),P_URL=character())

#We want our code to go through each webpage and extract the URL for each players overview page.

for (i in URLs) {  #for each item, or i, in our list called URLs
  #Everything between the { and the } will be run for each item within our list URLs.
  
  WS1 <- read_html(i) #assign WS1 with the information from each webpage. 
  #The URL changes each time R loops through the code as we allocate ?i? into the () of read_html.
  
  # scrape each player?s name from the club?s overview page
  Player <- WS1 %>%
    html_nodes("#sc_our_team a") %>%
    html_text() %>%
    as.character()
  
  
  P_URL <- WS1 %>%
    html_nodes("#sc_our_team a") %>%
    html_attr("href") %>%
    as.character()
  

  #Once the code collects all data we want to store this within the empty dataframe we created called Catcher1
  #The first step of doing this is by creating a simply temporary dataframe which we will use to quickly store the data for the team we scrapped. We call it ?temp?.
  #It will consist of the ?Player? variable and the P_URL variable.
  
  temp <- data.frame(Player,P_URL)
  
  #Now we are in a great position of having the data from the club?s page in a dataframe
  #BUT unless we save that data to our Catcher1 dataframe as the code loops through the next club it will simply overwrite
  #the ?temp? dataframe and we will be only left with the data from the last club in the loop
  
  Catcher1 <- rbind(Catcher1,temp)
  
  #Waiting Time indication that the loop is runing
  cat("*")
}


Catcher1$P_URL <- paste0("https://www.football.co.il",Catcher1$P_URL)


#- Scraping Players value from players Pages
Catcher2 <- data.frame(Player=character(),P_data=character())


for (i in Catcher1$P_URL) {
  
  WS2 <- read_html(i)
  P_data <- WS2 %>% 
          html_nodes(".player-details.col-xs-12") %>%
          html_text() %>%
          as.character()
  
  Player <- WS2 %>%
          html_nodes(".player-name") %>%
          html_text() %>%
          as.character()
  
  temp2 <- data.frame(Player,P_data)
  Catcher2 <- rbind(Catcher2,temp2)
  #-- beacuse some players dont have marketvalue we need if statement to skip them or the loop will break
  # if(length(MarketValue)>0){
  #   temp2 <- data.frame(Player,MarketValue)
  #   Catcher2 <- rbind(Catcher2,temp2)}
  # 
  # else {}
  cat("*")
  
}

Catcher2$Player <- as.character(Catcher2$Player)
Catcher2$P_data <- as.character(Catcher2$P_data)

Catcher2 <- Catcher2 %>% separate(P_data, c("Team", "Position", "Date"), sep=" \\| ")
Catcher2 <- Catcher2 %>% separate(Player, c("Name", "Number"), sep=" \\| ")
Catcher2$Team <- gsub(".*:","", Catcher2$Team)
Catcher2$Position <- gsub(".*:","", Catcher2$Position)
Catcher2$Date <- gsub(".*:","", Catcher2$Date)
Catcher2$Team <-trimws(Catcher2$Team)



## - TESTINGS ###


## Test for Player statistics -- WORKING ##
# url <- "https://www.football.co.il/player/45690"
# webpage <- read_html(url) 
# description <- html_nodes(webpage,'.player-page .stats-table .stats-row')
# html_text(description)
# m <- html_text(description)
# m <- trimws(m)
# l <- str_split(m, "\n")
# o <- unlist(l)
# p <- data.frame(o)
# 
# no.of.rows <- nrow(p)
# odd_indexes<-seq(from=1,to=no.of.rows,by=2)
# even_indexes<-seq(from=2,to=no.of.rows,by=2)
# attributes <- as.character(p[odd_indexes,])
# attributes_values <- as.character(p[even_indexes,])
# name <- html_nodes(webpage,".player-name") %>%
#   html_text() %>%
#   as.character()
# 
# team <- html_nodes(webpage, "div.col-xs-12.no-padding.player-top-info > a") %>%
#   html_attr("href") %>%
#   as.character()
# 
# # For first player
# df <- data.frame(name, team, attributes, attributes_values)
# 
# #df <- df[-1,]
# spread_df <- df %>% spread(attributes, attributes_values)
# 

#### BETA ####
## TRYING TO MAKE IT A LOOP ##
## WITHOUT NAME TO START WITH ###
Catcher3 <- data.frame(name=character(), team=character(), attributes=character(),attributes_values=character())

for(i in Catcher1$P_URL){
  
  WS3 <- read_html(i)
  
   P_name <- WS3 %>% html_nodes(".player-name") %>%
    html_text() %>%
    as.character()
  
  P_team <-  WS3 %>% html_nodes("div.col-xs-12.no-padding.player-top-info > a") %>%
    html_attr("href") %>%
    as.character()
 
  P_attr_names <- WS3 %>%
    html_nodes('.player-page .stats-table .stats-row') %>%
    html_text() %>%
    trimws() %>%
    str_split("\n") %>% 
    unlist() %>% 
    data.frame()
  
  
  P_attr_names2 <- as.character(P_attr_names[seq(from=1,to=nrow(P_attr_names),by=2),]) 
  
  P_attr_values <- WS3 %>%
    html_nodes('.player-page .stats-table .stats-row') %>%
    html_text() %>%
    trimws() %>%
    str_split("\n") %>% 
    unlist() %>% 
    data.frame() 
  
  P_attr_values2 <- as.character(P_attr_values[seq(from=2,to=nrow(P_attr_values),by=2),]) 


 temp3 <- data.frame(P_name, P_team, P_attr_names2, P_attr_values2)
  
 Catcher3 <- rbind(Catcher3, temp3)
 
 
 cat("*")
 
 
}

spread_Catcher3 <- Catcher3 %>% spread(P_attr_names2, P_attr_values2)



spread_Catcher3 <- spread_Catcher3 %>% separate(P_name, c("Name", "Number"), sep=" \\| ")
spread_merged <- merge(spread_Catcher3, Catcher2)
spread_merged$Position <- trimws(spread_merged$Position)

gk <- spread_merged %>% filter(Position == "????")
gk <- gk %>% select_if(~sum(!is.na(.)) > 0)
def <- spread_merged %>% filter(Position == "????")
def <- def %>% select_if(~sum(!is.na(.)) > 0)
mid <- spread_merged %>% filter(Position == "???")
mid <- mid %>% select_if(~sum(!is.na(.)) > 0)
st <- spread_merged %>% filter(Position == "????")
st <- st %>% select_if(~sum(!is.na(.)) > 0)




## TEST ##

library(rvest)
library(stringr)
library(plyr)
library(dplyr)
library(ggvis)
library(knitr)
options(digits = 4)


## Create a JS in phantom livrary
##https://www.datacamp.com/community/tutorials/scraping-javascript-generated-data-with-r

# // scrape_techstars.js
# 
# var webPage = require('webpage');
# var page = webPage.create();
# 
# var fs = require('fs');
# var path = 'techstars.html'
# 
# page.open('http://www.techstars.com/companies/stats/', function (status) {
#   var content = page.content;
#   fs.write(path,content,'w')
#   phantom.exit();
# });

system("./phantomjs scrape_israelifootball.js")

#Now you should have a local html file of the site

batches <- read_html("israelifootball.html") %>%
  html_nodes(".no-padding col-md-6 col-lg-3")

class(batches)

batch_titles <- batches %>%
  html_nodes("#playerListopponentGoal") %>%
  html_text()

batch_season <- str_extract(batch_titles, "(Fall|Spring|Winter|Summer)")
batch_year <- str_extract(batch_titles, "([[:digit:]]{4})")
# location info is everything in the batch title that is not year info or season info
batch_location <- sub("\\s+$", "",
                      sub("([[:digit:]]{4})", "",
                          sub("(Fall|Spring|Winter|Summer)","",batch_titles)))

# create data frame with batch info.
batch_info <- data.frame(location = batch_location,
                         year = batch_year,
                         season = batch_season)

breakdown <- lapply(batches, function(x) {
  company_info <- x %>% html_nodes(".parent")
  companies_single_batch <- lapply(company_info, function(y){
    as.list(gsub("\\[\\+\\]\\[\\-\\]\\s", "", y %>%
                   html_nodes("td") %>%
                   html_text()))
  })
  df <- data.frame(matrix(unlist(companies_single_batch),
                          nrow=length(companies_single_batch),
                          byrow=T,
                          dimnames = list(NULL, c("company","funding","status","hq"))))
  return(df)
})

# Add batch info to breakdown
batch_info_extended <- batch_info[rep(seq_len(nrow(batch_info)),
                                      sapply(breakdown, nrow)),]
breakdown_merged <- rbind.fill(breakdown)

# Merge all information
techstars <- tbl_df(cbind(breakdown_merged, batch_info_extended)) %>%
  mutate(funding = as.numeric(gsub(",","",gsub("\\$","",funding))))

