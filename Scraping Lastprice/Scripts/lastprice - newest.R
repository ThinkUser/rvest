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
#model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products<-data.frame(Product_name = model, old_price = old_price_data, price=price_data, url = url, stringsAsFactors = FALSE) 
products$Product_name <- as.character(products$Product_name)
products$url <- as.character(products$url)

## QE65Q6FN ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/92768904/%D7%98%D7%9C%D7%95%D7%95%D7%99%D7%96%D7%99%D7%94-65%27-Samsung/Samsung-QE65Q6FN'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## UE65NU8000 ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/21603982/%D7%98%D7%9C%D7%95%D7%99%D7%96%D7%99%D7%94-65%27%27-Samsung/Samsung-UE65NU8000'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## UE65NU7100 ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/31579568/%D7%98%D7%9C%D7%95%D7%95%D7%99%D7%96%D7%99%D7%94-65-Samsung/Samsung-UE65NU7100'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## LG65SK8500Y ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/37367989/%D7%98%D7%9C%D7%95%D7%95%D7%99%D7%96%D7%99%D7%94-65-LG/LG-65SK8500Y'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## LG65UK7500P ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/100003867/%D7%98%D7%9C%D7%95%D7%99%D7%96%D7%99%D7%94-65-LG/LG-65UK7500P'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## Tadiran-Swift-Plus-10 ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/79043252/%D7%9E%D7%96%D7%92%D7%9F-%D7%A2%D7%99%D7%9C%D7%99-Tadiran/Tadiran-Swift-Plus-10A'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')
#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
old_price_data <- as.numeric(old_price_data)
old_price_data <- old_price_data[1]
price <- html_nodes(webpage,'.col-md-9 .price')

#Converting the ranking data to text 
price_data <- html_text(price)
price_data <- gsub("\r","",price_data) 
price_data <- gsub("\n","",price_data) 
price_data <- gsub("\t","",price_data) 
price_data <- gsub('\\D+','', price_data)
price_data <- price_data[1]
price_data <- substr(price_data,0,4)
price_data <- as.numeric(price_data)
products <- rbind(products, c(model, old_price_data, price_data, url))

## Tadiran-Swift-Plus-14 ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/73541848/%D7%9E%D7%96%D7%92%D7%9F-%D7%A2%D7%99%D7%9C%D7%99-Tadiran/Tadiran-Swift-plus-14A'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))

## Bosch SMV45AX00E ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/77520272/%D7%9E%D7%93%D7%99%D7%97-%D7%9B%D7%9C%D7%99%D7%9D-%D7%A8%D7%97%D7%91-%D7%90%D7%99%D7%A0%D7%98%D7%92%D7%A8%D7%9C%D7%99-Bosch/BOSCH-SMV45AX00E'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))


## Bloomberg KQD1616X ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/14877602/%D7%9E%D7%A7%D7%A8%D7%A8-4-%D7%93%D7%9C%D7%AA%D7%95%D7%AA-Blomberg/Blomberg-KQD1616X'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))


## Bloomberg KQD1251IN  ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/19949/%D7%9E%D7%A7%D7%A8%D7%A8-4-%D7%93%D7%9C%D7%AA%D7%95%D7%AA--522-%D7%9C%D7%99%D7%98%D7%A8-Blomberg/Blomberg-KQD1251IN-%D7%A0%D7%99%D7%A8%D7%95%D7%A1%D7%98%D7%94'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))



## Bloomberg KQD1620GW  ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/95407388/%D7%9E%D7%A7%D7%A8%D7%A8-4-%D7%93%D7%9C%D7%AA%D7%95%D7%AA-Blomberg/Blomberg-KQD1620GW'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))



## Bloomberg KQD1621  ####
## Url to scrape
url <-'https://www.lastprice.co.il/p/42177255/%D7%9E%D7%A7%D7%A8%D7%A8-4-%D7%93%D7%9C%D7%AA%D7%95%D7%AA-Blomberg/Blomberg-KQD1621'
webpage <- read_html(url) 
description <- html_nodes(webpage,'.main-text')
model <- html_text(description)
model <- model[1]
model <- as.character(model)

#Using CSS selectors to scrap the old price
old_price <- html_nodes(webpage,'.old-price')

#Converting the ranking data to text 
old_price_data <- html_text(old_price)
old_price_data <- gsub("₪","",old_price_data)
old_price_data <- gsub(",","", old_price_data)
old_price_data <- gsub("\244","", old_price_data)
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
products <- rbind(products, c(model, old_price_data, price_data, url))



## Add Date ####
products$date <- Sys.Date()

## Create a csv file ####
filename <-paste("lastprice",
                 Sys.Date(),
                 paste("products", collapse = "-",sep=""),
                 ".csv",sep="-")
write.csv(products, filename)

## Send csv file in Email ####
library(rJava)
library(mailR)
msg <- paste("Hi,","","This is an autmated email from R about lastprice,","Erez")
sender <- "mightydu@gmail.com"

# Define who should get your email
recipients <- c("erezlouzon@gmail.com",
                "erez@thinkuser.co.il",
                "hila.myway@gmail.com")

# Send your email with the send.mail function
## If email FAILS turn on this https://myaccount.google.com/lesssecureapps
send.mail(from = sender,
          to = recipients,
          subject = "lastprice prices",
          body = msg,
          smtp = list(host.name = "smtp.gmail.com", port = 587,
                      user.name = "mightydu@gmail.com",
                      passwd = "spacewalker", ssl = TRUE),
          # attach.files = c("./download.log", "upload.log"),
          attach.files = c(filename),
          authenticate = TRUE,
          send = TRUE)