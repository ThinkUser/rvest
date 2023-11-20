#-- Scrape Paneco prices --#

library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)

##################################################
#### CHoose a mentod, Manual , loops or purrr ####
##################################################

####Manual WITHOUT LOOPS ####
#-- Move after comments for loops --#

# #-- Dogajolo --#
# ## Url to scrape
# url <-'https://www.paneco.co.il/%D7%93%D7%95%D7%92%D7%99%D7%95%D7%9C%D7%95-%D7%A8%D7%95%D7%A1%D7%95-%D7%A1%D7%95%D7%A4%D7%A8-%D7%98%D7%95%D7%A1%D7%A7%D7%9F-2015'
# webpage <- read_html(url) 
# description <- html_nodes(webpage,'.product-details-page .product-name')
# model <- html_text(description)
# model <- model[1]
# model <- as.character(model)
# model <- gsub("\r\n", "", model)
# model <-gsub("[^A-Za-z0-9 ]","",model)
# model <- trimws(model)
# 
# #Using CSS selectors to scrap the old price
# price <- html_nodes(webpage,'.product-page .overview .product-price span')
# 
# #Converting the ranking data to text 
# price <- html_text(price)
# price <- price[4]
# price <- gsub("\r\n", "", price)
# price <- trimws(price)
# price <- as.numeric(price)
# 
# products<-data.frame(Product_name = model, price = price, url = url, stringsAsFactors = FALSE) 
# products$Product_name <- as.character(products$Product_name)
# products$url <- as.character(products$url)
# 
# 
# #-- Yarden Cabarnet --#
# 
# url <-'https://www.paneco.co.il/yarden-cabernet-sauvignon'
# webpage <- read_html(url) 
# description <- html_nodes(webpage,'.product-details-page .product-name')
# model <- html_text(description)
# model <- model[1]
# model <- as.character(model)
# model <- gsub("\r\n", "", model)
# model <-gsub("[^A-Za-z0-9 ]","",model)
# model <- trimws(model)
# 
# #Using CSS selectors to scrap the old price
# price <- html_nodes(webpage,'.product-page .overview .product-price span')
# 
# #Converting the ranking data to text 
# price <- html_text(price)
# price <- price[4]
# price <- gsub("\r\n", "", price)
# price <- trimws(price)
# price <- as.numeric(price)
# 
# products$Product_name <- as.character(products$Product_name)
# products$url <- as.character(products$url)
# products <- rbind(products, c(model, price, url))
# 
# 
# 
# 
# #-- Campari --#
# 
# url <-'https://www.paneco.co.il/campari-bitters-1l'
# webpage <- read_html(url) 
# description <- html_nodes(webpage,'.product-details-page .product-name')
# model <- html_text(description)
# model <- model[1]
# model <- as.character(model)
# model <- gsub("\r\n", "", model)
# model <-gsub("[^A-Za-z0-9 ]","",model)
# model <- trimws(model)
# 
# #Using CSS selectors to scrap the old price
# price <- html_nodes(webpage,'.product-page .overview .product-price span')
# 
# #Converting the ranking data to text 
# price <- html_text(price)
# price <- price[4]
# price <- gsub("\r\n", "", price)
# price <- trimws(price)
# price <- as.numeric(price)
# 
# products$Product_name <- as.character(products$Product_name)
# products$url <- as.character(products$url)
# products <- rbind(products, c(model, price, url))
# 
# 
# #-- Aperol --#
# 
# url <-'https://www.paneco.co.il/%D7%90%D7%A4%D7%A8%D7%95%D7%9C-%D7%9C%D7%99%D7%98%D7%A8-%D7%9B%D7%A9%D7%A8'
# webpage <- read_html(url) 
# description <- html_nodes(webpage,'.product-details-page .product-name')
# model <- html_text(description)
# model <- model[1]
# model <- as.character(model)
# model <- gsub("\r\n", "", model)
# model <-gsub("[^A-Za-z0-9 ]","",model)
# model <- trimws(model)
# 
# #Using CSS selectors to scrap the old price
# price <- html_nodes(webpage,'.product-page .overview .product-price span')
# 
# #Converting the ranking data to text 
# price <- html_text(price)
# price <- price[4]
# price <- gsub("\r\n", "", price)
# price <- trimws(price)
# price <- as.numeric(price)
# 
# products$Product_name <- as.character(products$Product_name)
# products$url <- as.character(products$url)
# products <- rbind(products, c(model, price, url))



###############
#### Loops ####
###############

#-- Lets try a for loop
urls <- c("https://www.paneco.co.il/%D7%99%D7%A8%D7%93%D7%9F-%D7%A1%D7%99%D7%A8%D7%94", #Yarden Syrah
          "https://www.paneco.co.il/har-odem-odem-forest",
          "https://www.paneco.co.il/%D7%95%D7%99%D7%A0%D7%95-%D7%A0%D7%95%D7%91%D7%99%D7%9C%D7%94-%D7%93%D7%99-%D7%9E%D7%95%D7%A0%D7%98%D7%A4%D7%95%D7%9C%D7%A6%D7%90%D7%A0%D7%95",
          "https://www.paneco.co.il/%D7%9B%D7%A8%D7%9E%D7%9C-%D7%95%D7%99%D7%A0%D7%99%D7%90%D7%A8%D7%93%D7%A1-%D7%A1%D7%99%D7%A8%D7%94-%D7%9E%D7%95%D7%A8%D7%91%D7%93%D7%A8",
          "https://www.paneco.co.il/%D7%99%D7%A4%D7%95-%D7%90%D7%99%D7%9E%D7%90%D7%96-2104",
          "https://www.paneco.co.il/or-haganuz-elima",
          "https://www.paneco.co.il/galil-mountain-yiron",
          "https://www.paneco.co.il/catalog/product/view/id/1994/s/or-haganuz-merom-vineyard-cabarnet-franc/category/477/", # NOT WORKING
          "https://www.paneco.co.il/%D7%99%D7%A8%D7%93%D7%9F-%D7%A9%D7%A8%D7%93%D7%95%D7%A0%D7%94", # Yarden Chardonay Not Working
          "https://www.paneco.co.il/%D7%94%D7%A8-%D7%90%D7%95%D7%93%D7%9D-%D7%99%D7%A2%D7%A8-%D7%9C%D7%91%D7%9F", #Har Odem White NOT WORKING
          "https://www.paneco.co.il/%D7%99%D7%A7%D7%91-%D7%A8%D7%9E%D7%AA-%D7%A0%D7%92%D7%91-%D7%A9%D7%A8%D7%93%D7%95%D7%A0%D7%94-%D7%A0%D7%95%D7%95%D7%94-%D7%9E%D7%93%D7%91%D7%A8", # RN Chardonay NOT WORKING
          "https://www.paneco.co.il/%D7%99%D7%A7%D7%91-%D7%A8%D7%9E%D7%AA-%D7%A0%D7%92%D7%91-%D7%A4%D7%99%D7%A0%D7%95-%D7%92%D7%A8%D7%99-%D7%A0%D7%95%D7%95%D7%94-%D7%9E%D7%93%D7%91%D7%A8") #RN Pino Grey 

dummy <- data.frame(Product_name = character(), price = numeric(), url = character(), stringsAsFactors = FALSE)

for(i in urls){
  
  WS1 <- read_html(i)                         
  
  description <- WS1 %>% html_nodes('.base') %>% html_text() %>% as.character()
  price <- WS1 %>%  html_nodes('.product-info-price .price') %>% html_text() 
  url <- urls[i]
  temp <- data.frame(description, price, url)
  dummy <- rbind(dummy,temp, url)
  cat("*")
  
}

#-- We want only the odd rows
# no.of.rows <- nrow(dummy)
# odd_indexes<-seq(1,no.of.rows,2)
# dummy <- data.frame(dummy[odd_indexes,])
# dummy$description<- gsub("\r\n","", dummy$description)
# #dummy$description <- gsub("[^A-Za-z0-9 ]","",dummy$description)
# dummy$description <- trimws(dummy$description)
# dummy$price<- gsub("₪","", dummy$price)
# dummy$price <-trimws(dummy$price)
# #dummy$price <- as.numeric(dummy$price)


# Select where description is available
dummy <- dummy %>% filter(!is.na(description))
dummy$price<- gsub("₪","", dummy$price)

#-- Add Target Prices
targets <- c(80, 85, 105, 120, 105, 75)
dummy$target <- targets

dummy <- dummy %>% mutate(distance = price/target)



###################################
#### The func way and purrr !! ####
###################################

library(tidyverse)
library(magrittr)
library(purrr)
library(glue)
library(stringr)
library(rvest)
library(xml2)


paneco <- function(link){
  
  webpage <- glue("https://www.paneco.co.il/{link}")
  description <-  webpage %>%
    read_html() %>%
    html_nodes('.product-name') %>%
    html_text() %>%
    as.character()
  
  description <- gsub("\r\n", "", description)
  description <-gsub("[^A-Za-z0-9 ]","",description)
  description <- trimws(description)
  
  price <- webpage %>%
    read_html() %>%
    html_nodes('.product-price>span') %>%
    html_text()
  
  #price <- price[4]
  price <- gsub("\r\n", "", price)
  price <- trimws(price)
  price <- as.numeric(price)
  price <- na.omit(price)
  
  df <- tibble(Name = description,
               # extract text title
               Price = price,
               Link = webpage,
               Date = Sys.Date()
  )
  
  # Lyrics = map_chr(Link,get_lyrics))
  
  
  return(df)
}

links <- c("campari-bitters-1l",
           "%D7%93%D7%95%D7%92%D7%99%D7%95%D7%9C%D7%95-%D7%A8%D7%95%D7%A1%D7%95-%D7%A1%D7%95%D7%A4%D7%A8-%D7%98%D7%95%D7%A1%D7%A7%D7%9F-2015",
           "yarden-merlot-2",
           "yarden-cabernet-sauvignon",
           "c-blanc-du-castel-2015",
           "galil-mountain-yiron",
           "southern-comfort-4",
           "baileys-irish-cream",
           "midori-melon-1l",
           "%D7%A4%D7%A8%D7%A0%D7%94-%D7%91%D7%A8%D7%A0%D7%A7%D7%94",
           "aperol-1l",
           "grey-goose-vodka-6x1000ml-40-3",
           "three-olives-cake",
           

             
)

df <- map_df(links, paneco)

assign(paste0(Sys.Date(), ""), value = df)

#### Alerts ####
if (dummy$distance > 1 ){
  
  library(rJava)
  library(mailR)
  
  # Write the content of your message
  msg <- paste("Hi,","","there was a change in price")
  
  # Define who the sender is
  sender <- "mightydu@gmail.com"
  
  # Define who should get your email and/or sms 
  ##recipients <- c("erez@thinkuser.co.il", "972547369919@textmagic.com" )
  recipients <- c("972547369919@textmagic.com" )
  
  # Send your email with the send.mail function
  ## If email FAILS turn on this https://myaccount.google.com/lesssecureapps
  
  send.mail(from = sender,
            to = recipients,
            subject = "your daily ThinkSmS",
            body = msg,
            smtp = list(host.name = "smtp.gmail.com", port = 587,
                        user.name = "mightydu@gmail.com",
                        passwd = "spacewalker", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)}