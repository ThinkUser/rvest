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

system("./phantomjs scrape_techstars.js")

#Now you should have a local html file of the site

batches <- read_html("techstars.html") %>%
  html_nodes(".batch")

class(batches)

batch_titles <- batches %>%
  html_nodes(".batch_class") %>%
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