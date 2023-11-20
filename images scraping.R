library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
library(tidylog)
library(rvest)
library(googleAnalyticsR)
library(googleAuthR)

options(scipen=10000)
gar_auth("tu.httr-oauth")
account_list <- ga_account_list()
meta <- ga_meta()
mac_id <- 24939694
as.Date(start <- "2019-10-01")
as.Date(end <- "2019-10-31")

mac <- google_analytics(mac_id, date_range = c(start, end),
                        dimensions = c("pagePath"),
                        metrics = c("pageViews"),
                        max = -1,
                        anti_sample = TRUE)

glimpse(mac)

mac2 <- mac %>% group_by(pagePath) %>% summarize(n=n_distinct(pagePath))
mac3 <- mac2 %>% filter(str_detect(pagePath, regex(".*/zappashow/.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*translate.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(str_detect(pagePath, regex(".*www.zappa-club.co.il.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*googleweblight.com.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*webcache.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*=4_61258_.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*zappaartist.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*en_EN.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex(".*/en/.*", ignore_case = TRUE)))
mac3 <- mac3 %>% filter(!str_detect(pagePath, regex("^www.zappa-club.co.il/zappashow//.*", ignore_case = TRUE)))



#mac3 <- mac3 %>% filter(!str_detect(pagePath, regex("^/$", ignore_case = TRUE)))

mac3$pagePath <- str_replace_all(mac3$pagePath, ".*_atscid","")
mac3$pagePath <- str_replace_all(mac3$pagePath, ".*/?showcontext","")
mac3$pagePath <- str_replace_all(mac3$pagePath, ".*=true","")
mac3$pagePath <- str_replace_all(mac3$pagePath, ".*zappashow//","zappashow/")

mac3$n <- NULL
glimpse(mac3)
mac3 <- mac3 %>% group_by(pagePath)
mac4 <- mac3 %>% mutate(hostname = "https://", full_url= paste0(hostname,pagePath)) 
mac4 <- mac4 %>% group_by(pagePath)
mac5 <- mac4 %>% select(full_url)

glimpse(mac5)


Catcher1 <- data.frame(P_URL=character(), image=character())
URLs <- mac5$full_url




for(i in URLs) {
  WS1 <- read_html(i)
  image <- WS1 %>%
    html_nodes(xpath = '//meta[@property="og:image"]') %>% 
    html_attr('content') 
  
  P_URL <- paste0(i)
  if(length(image)>0){
    temp <- data.frame(P_URL, image)
    Catcher1 <- rbind(Catcher1,temp)
  }
  else{
    image <- "No Image - Check the link"
    temp <- data.frame(P_URL, image)
    Catcher1 <- rbind(Catcher1,temp)
  }

  cat("*")
  }

  
  



for(i in URLs) {
  tryCatch({
    
  WS1 <- read_html(i)
  image <- WS1 %>%
    html_nodes(xpath = '//meta[@property="og:image"]') %>% 
    html_attr('content') 
  
  P_URL <- paste0(i)
  if(length(image)>0){
    temp <- data.frame(P_URL, image)
    Catcher1 <- rbind(Catcher1,temp)
  }
  else{
    image <- "No Image - Check the link"
    temp <- data.frame(P_URL, image)
    Catcher1 <- rbind(Catcher1,temp)
  }
  
  cat("*")
}, error = function(e) {
  message("Had a problem at iteration ", i, ": ", conditionMessage(e))
})
  next()
}




image_df <- Catcher1
names <- c("pagePath", "image_url")
names(image_df) <- names



table1 <- google_analytics(mac_id, date_range = c(start, end),
                           dimensions = c("pagePath"),
                           metrics = c("pageViews"),
                           max = -1,
                           anti_sample = TRUE)
table2 <- image_df
#table3 <- mac %>% filter(str_detect(pagePath, regex(".*/product/.*", ignore_case = TRUE)))
table1 <- table1 %>% mutate(hostname = "https://", full_url= paste0(hostname,pagePath)) 
table1$pagePath <- table1$full_url
table1 <- table1 %>% select(pagePath, pageViews)

o <- left_join(table1, table2, by="pagePath")
o <- o %>% filter(!is.na(image_url))
final <- o
# table3 <- table3 %>% rename(P_URL = pagePath)
# table2 <- table2 %>% rename(P_URL = product_name)
# 
# 
# table2$P_URL <- table2$P_URL %>% str_replace_all("https://www.maccosmetics.co.il","")
# joined_2_3 <- left_join(table3, table2, by="P_URL")
# joined_2_3 <- joined_2_3 %>% select(productName, P_URL, image_url)
# final <- left_join(table1, joined_2_3, by="productName")


#############################
## Uploading to GCS and BQ ##
#############################

#-- Setting up BigQuery and GCS --#
library(googleCloudStorageR)
library(bigQueryR)

#-- Setting Project --#
bqr_global_project("erez-bigquery")
bqr_get_global_project()

#-- get project list --#
projects <- bqr_list_projects()
head(projects)

my_project <- projects[2,1]
my_project_id <- projects[2,3]

#-- for first project, get datasets --#
datasets <- bqr_list_datasets(my_project)
head(datasets)
my_dataset <- datasets[1]
head(my_dataset)

#####################
#### Upload Data ####
#####################

#-- Get Buckets --#
gcs_list_buckets("stellar-vista-149518")
gcs_list_buckets("erez-bigquery")

#-- Set bucket to store the data --#
gcs_global_bucket("erez-bigquery-storage-bucket")

###########################
####  GCS Upload Data ####
##########################

#-- Upload To Storage --#
f <- function(input, output) {
  write.table(input, sep = ",", col.names = FALSE, row.names = FALSE, 
              quote = FALSE, file = output, qmethod = "double")}

filename=paste0("zappa_test.csv")
gcs_upload(final, name = filename, object_function = f)
#gcs_upload(df, name = "easydf.csv", object_function = f)


#-- upload to BQ --#

path=paste0("gs://erez-bigquery-storage-bucket/",filename)
user_schema <- schema_fields(final)
bqr_upload_data(projectId = my_project_id, 
                datasetId = "ga_gcs_bq_r", 
                tableId = "zappa", 
                upload_data = c(path), 
                create = c("CREATE_IF_NEEDED"),
                overwrite = TRUE,
                schema = user_schema)

