#-- Scraping TripAdvisor --#
#-- https://www.worldfullofdata.com/basics-web-scraping-r-rvest/ --#

library(rvest)
library(tidyverse)
library(tidylog)

# Read the web page and get the number of reviews
tripadvisor_restaurant <- read_html("https://www.tripadvisor.com/Restaurant_Review-g60763-d457808-Reviews-Daniel-New_York_City_New_York.html")

totalReviews <- tripadvisor_restaurant %>%
  html_nodes(".reviewCount") %>%
  html_text()

totalReviews <- totalReviews[1]
totalReviews <- strsplit(totalReviews, " ")[[1]][1]
totalReviews <- as.numeric(gsub(",", "", totalReviews))


# Get the cuisine and location of the restaurant
cuisine <- tripadvisor_restaurant %>%
  html_node(".cuisines .text") %>%
  html_text()
cuisine

#[1] " French, Vegetarian Friendly, Vegan Options, Gluten Free Options "

streetAddress <- tripadvisor_restaurant %>%
  html_node("span.street-address") %>%
  html_text()
streetAddress
#[1] "60 East 65th Street"

locality <- tripadvisor_restaurant %>%
  html_node("span.locality") %>%
  html_text()
locality
#[1] "New York City, NY 10065"

name <- tripadvisor_restaurant %>%
  html_node(".ui_header") %>%
  html_text()
name

# Get the text of the services and its rating
ratingServiceText <- tripadvisor_restaurant  %>%
  html_nodes(".restaurants-detail-overview-cards-RatingsOverviewCard__overallRating--nohTl") %>%
  html_text()
ratingServiceText
#4.5

## FROM IMAGE GET ATTRIBUTE !!!
ratingServiceValue <- tripadvisor_restaurant %>%
  html_nodes(".ui_bubble_rating") %>%
  html_attr("alt")
ratingServiceValue

#[1] "4.5 of 5 bubbles" "4.5 of 5 bubbles" "4.0 of 5 bubbles" "4.5 of 5 bubbles"

ratingServiceValue <- strsplit(ratingServiceValue, " ")
ratingServiceValue <- as.numeric(lapply(ratingServiceValue, `[[`, 1))
ratingServiceValue
#[1] 4.5 4.5 4.0 4.5

#-- combine all the scraped data into a data frame with one row.
record_restaurant <- data.frame(name = name,
                                locality = locality,
                                streetAddress = streetAddress,
                               # cuisine = cuisine,
                             #   ratingOverall = overallRating,
                                ratingService = as.list(setNames(ratingServiceValue, ratingServiceText)),
                              #  ratingOverall = as.list(setNames(ratingValue, ratingText)),
                                totalReviews = totalReviews
)

#LOOP THROUGH OBJECTS AND PAGES

# Read html and select the URL's for each restaurant
tripadvisor_home <- read_html("https://www.tripadvisor.com/Restaurants-g60763-New_York_City_New_York.html")

restaurant_URLs <- tripadvisor_home %>%
  html_nodes(".restaurants-list-ListCell__restaurantName--2aSdo") %>%
  html_attr("href")
restaurant_URLs


#loop through the 30 URL’s to get the information per restaurant .
# Loop through 30 restaurants and combine this into one data frame
library(plyr) # needed for rbind.fill function

get30Restaurants <- function(restaurant_URLs){
  data <- data.frame()
  i=1
  for(i in 1:length(restaurant_URLs)){
    tripadvisor_restaurant <- read_html(paste0("https://www.tripadvisor.com", restaurant_URLs[i]))
    record_restaurant <- getRestaurant(tripadvisor_restaurant)
    data <- rbind.fill(data, record_restaurant)
    print(i)
  }
  data
}

df30Restaurants <- get30Restaurants(restaurant_URLs)


#------------

library(rvest)
library(plyr)

tripadvisor_home <- html_session("https://www.tripadvisor.com/Restaurants-g60763-New_York_City_New_York.html")

getRestaurant <- function(tripadvisor_restaurant){
  name <- tripadvisor_restaurant %>%
    html_nodes(".heading_title") %>%
    html_text()
  
  overallRating <- tripadvisor_restaurant %>%
    html_nodes(".overallRating") %>%
    html_text() %>%
    as.numeric()
  
  totalReviews <- tripadvisor_restaurant %>%
    html_nodes(".seeAllReviews") %>%
    html_text()
  totalReviews <- strsplit(totalReviews, " ")[[1]][1]
  totalReviews <- as.numeric(gsub(",", "", totalReviews))
  
  streetAddress <- tripadvisor_restaurant %>%
    html_node(".address .street-address") %>%
    html_text()
  
  locality <- tripadvisor_restaurant %>%
    html_node(".address .locality") %>%
    html_text()
  
  cuisine <- tripadvisor_restaurant %>%
    html_node(".cuisines .text") %>%
    html_text()
  
  ratingText <- tripadvisor_restaurant %>%
    html_nodes("#ratingFilter .row_label") %>%
    html_text()
  
  ratingValue <- tripadvisor_restaurant %>%
    html_nodes("#ratingFilter span") %>%
    html_text()
  ratingValue <- as.numeric(ratingValue[seq(4, length(ratingValue), 5)])
  
  ratingServiceText <- tripadvisor_restaurant %>%
    html_nodes(".ratingSummary .text") %>%
    html_text()
  
  ratingServiceValue <- tripadvisor_restaurant %>%
    html_nodes(".ratingSummary .ui_bubble_rating") %>%
    html_attr("alt")
  ratingServiceValue <- strsplit(ratingServiceValue, " ")
  ratingServiceValue <- as.numeric(lapply(ratingServiceValue, `[[`, 1))
  
  record_restaurant <- data.frame(name = name,
                                  locality = locality,
                                  streetAddress = streetAddress,
                                  cuisine = cuisine,
                                  ratingOverall = overallRating,
                                  ratingService = as.list(setNames(ratingServiceValue, ratingServiceText)),
                                  ratingOverall = as.list(setNames(ratingValue, ratingText)),
                                  totalReviews = totalReviews
  )  
  record_restaurant
}

get30Restaurants <- function(restaurant_URLs){
  data <- data.frame()
  i=1
  for(i in 1:length(restaurant_URLs)){
    tripadvisor_restaurant <- read_html(paste0("https://www.tripadvisor.com", restaurant_URLs[i]))
    record_restaurant <- getRestaurant(tripadvisor_restaurant)
    data <- rbind.fill(data, record_restaurant)
    print(i)
  }
  data
}

get30RestaurantsXpages <- function(tripadvisor_home, X){
  data <- data.frame()
  i = 1
  for(i in 1:X){
    if(i != 1){ # Go to next page but don't skip the first page
      next_URL <- tripadvisor_home %>%
        html_nodes(".nav.next") %>%
        html_attr("href")
      tripadvisor_home <- jump_to(tripadvisor_home, paste0("https://www.tripadvisor.com", next_URL))
    }
    restaurant_URLs <- tripadvisor_home %>%
      html_nodes(".property_title") %>%
      html_attr("href")
    df30Restaurants <- get30Restaurants(restaurant_URLs)
    data <- rbind.fill(data, df30Restaurants)
    print(paste0("Page ", i))
  }
  data
}

restaurants <- get30RestaurantsXpages(tripadvisor_home, 2)

