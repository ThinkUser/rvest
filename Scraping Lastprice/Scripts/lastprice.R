## Scrape last price prices ##

library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)

## QE65Q7FN ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/62942396/%D7%98%D7%9C%D7%95%D7%99%D7%96%D7%99%D7%94-65%27-Samsung/Samsung-QE65Q7FN'
webpage <- read_html(url) 

description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)
model <- levels(model)
#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')
#Converting the ranking data to text 
old_price_data <- html_text(old_price)

old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]

price <- html_nodes(webpage,'.col-md-9 .price')
#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)

as.Date(date <- Sys.Date())
products<-data.frame(Product_name = model, old_price = old_price_data, price=price_data, date=date, url = url) 



## Tadiran-Swift-Plus-10 ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/79043252/%D7%9E%D7%96%D7%92%D7%9F-%D7%A2%D7%99%D7%9C%D7%99-Tadiran/Tadiran-Swift-Plus-10A'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model, stringsAsFactors=FALSE)
model <- levels(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')
#Converting the ranking data to text 
old_price_data <- html_text(old_price)

old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]

price <- html_nodes(webpage,'.col-md-9 .price')
#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)



rbind(products, c(model, old_price_data, price_data, date, url))





