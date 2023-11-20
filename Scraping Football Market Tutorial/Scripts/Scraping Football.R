# A Total Beginners Guide to Web Scraping Football Data 
#-- Part 1+2: https://sportsdatachallenge.wordpress.com/2016/09/21/a-total-beginners-guide-to-web-scraping-football-data-part-1/
#-- https://sportsdatachallenge.wordpress.com/2016/09/22/a-total-beginners-guide-to-web-scraping-football-data-part-2/

library(rvest)
library(tidyverse)

URL <- "http://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1"
WS <- read_html(URL)

#-- Get all the clubs urls to scrape from the premier league page
URLs <- WS %>%
        html_nodes(".hide-for-pad .vereinprofil_tooltip") %>%
        html_attr("href") %>%
        as.character()

#-- Links are missing the domain so we'll add them
 URLs <- paste0("http://www.transfermarkt.com",URLs)

#--LOOOOPSSSSSSS
#-- he information we will be extracting is the URL address of all players with each squad. 
#-- Leaving us with a list of 516 EPL players URL addresses for their personal overview pages

#-- Setup the Empty Data Frame to Store Data
Catcher1 <- data.frame(Player=character(),P_URL=character())

#We want our code to go through each webpage and extract the URL for each players overview page.

for (i in URLs) {  #for each item, or i, in our list called URLs
                    #Everything between the { and the } will be run for each item within our list URLs.
  
  WS1 <- read_html(i) #assign WS1 with the information from each webpage. 
                      #The URL changes each time R loops through the code as we allocate ?i? into the () of read_html.
  
  # scrape each player?s name from the club?s overview page
  Player <- WS1 %>%
            html_nodes("#yw1 .spielprofil_tooltip") %>%
            html_text() %>%
            as.character()
  
 
  P_URL <- WS1 %>%
          html_nodes("#yw1 .spielprofil_tooltip") %>%
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


no.of.rows <- nrow(Catcher1)
odd_indexes<-seq(1,no.of.rows,2)
Catcher1 <- data.frame(Catcher1[odd_indexes,])
Catcher1$P_URL <- paste0("http://www.transfermarkt.com",Catcher1$P_URL)
                         

 #-- PART 3 - More Loops
 #-- https://sportsdatachallenge.wordpress.com/2016/10/04/loop-through-the-player-links-and-collect-the-name-and-tm-value-of-the-players/
 #- Scraping Players value from players Pages
Catcher2 <- data.frame(Player=character(),MarketValue=character())


 for (i in Catcher1$P_URL) {
   
   WS2 <- read_html(i)
   MarketValue <- WS2 %>%
     html_nodes(".dataMarktwert a") %>%
     html_text() %>%
     as.character()
   
   Player <- WS2 %>%
     html_nodes("h1 b") %>% 
     html_text() %>%
     as.character()
   
   #-- beacuse some players dont have marketvalue we need if statement to skip them or the loop will break
   if(length(MarketValue)>0){
   temp2 <- data.frame(Player,MarketValue)
   Catcher2 <- rbind(Catcher2,temp2)}
   
  else {}
   cat("*")
   
 }
 

Catcher2$Player <- as.character(Catcher2$Player)
Catcher2$MarketValue <- as.character(Catcher2$MarketValue)
Catcher2$MarketValue <- strsplit(Catcher2$MarketValue, " ")



test <- strsplit(Catcher2$MarketValue, "?")
