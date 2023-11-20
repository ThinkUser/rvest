## https://rpubs.com/Radcliffe/superbowl
## Web scraping in R: A tutorial using Super Bowl Data

library(rvest)
library(stringr)
library(tidyr)

## use the read_html function to read a web page
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url)

##use the functions html_nodes and html_table, extract the HTML table element and convert it to a data frame.
sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]

## Inspect
head(sb)

##remove the first two rows, and set the column names.
sb <- sb[-(1:2), ]
names(sb) <- c("number", "date", "site", "result")

#traditional to use Roman numerals to refer to Super Bowls, but Arabic numerals are more convenient to work with. We will also convert the date to a standard format.

sb$number <- 1:51
sb$date <- as.Date(sb$date, "%B. %d, %Y")  ##use ?strptime for details about %B. %d, %Y
head(sb)

## The result column should be split into four columns
## The winning team’s name, the winner’s score, the losing team’s name, and the loser’s score.
## We start by splitting the results column into two columns at the comma.
## This operation uses the separate function from the tidyr package.

sb <- separate(sb, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sb)

## split off the scores from the winner and loser columns.
## The function str_extract from the stringr package finds a substring matching a pattern.
## In this case, the pattern is a sequence of 1 or more digits at the end of a line

pattern <- " \\d+$" ## This is plain regex 
sb$winnerScore <- as.numeric(str_extract(sb$winner, pattern))
sb$loserScore <- as.numeric(str_extract(sb$loser, pattern))
sb$winner <- gsub(pattern, "", sb$winner)
sb$loser <- gsub(pattern, "", sb$loser)
head(sb)

## Reorder Columns

sb <- select(sb, number, date, site, winner, winnerScore, loser, loserScore)





