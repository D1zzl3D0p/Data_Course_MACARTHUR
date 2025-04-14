library(polite)
library(rvest)
library(purrr)
library(janitor)

library(tidyverse)

# Setting up variables -------

rdata <- read.csv("Final Project/streaming_service.csv")
# get the streaming data
stream <- rdata |>
  # you have to use paste0 because default paste adds a space in between
  mutate(date=as.POSIXct(paste0("01-",date),format='%d-%b-%Y'))|> 
  mutate(service = as.factor(service))

# get the days that we need to search through
days <- seq.Date(as.Date(min(stream$date)),as.Date(max(stream$date)),by="month")

# https://web.archive.org/web/20160201231417/https://thepiratebay.se/top/48hall
host <- "https://archive.org/"

extension <- c("https://thepiratebay.org/top/48hall","https://thepiratebay.org/search.php?q=top100:all")
# https://thepiratebay.org/top/48hall from around 2021 to 2024
# https://thepiratebay.org/search.php?q=top100:all from before 2021

piracy = data.frame()


# Querying Internet Archive --------------------------------------------------



# using the polite package to read and obey the robots.txt
# use the session for rvest stuff, use polite_GET instead of 
# httr::GET, we will use both eventually
session <- bow(host)
polite_GET <- politely(httr::GET,delay=1, verbose=TRUE) 

dayframe <- data.frame(days, old_url=NA,new_url=NA)

# create a shorter test dataset to test with
sdayframe <- head(dayframe,20)

# example of how to use rvest with polite
# current_page <- nod(session, "https://archive.org/wayback/available?url=https://thepiratebay.org/search.php?q=top100:all&timestamp=20250329181544")|>
#   scrape()

# example of how to use httr::GET politely (we defined polite_GET above)
# test <- polite_GET("https://archive.org/wayback/available?url=https://thepiratebay.org/search.php?q=top100:all&timestamp=20250329181544")

# how to extract the content from the GET into human legible format
# print(rawToChar(test$content))

# grab closest timestamp
# check to see if timestamp is within target month
# get request

base_url = "https://archive.org/wayback/available?url="

# get responses back from the internet archive, trying to see the closest
# available timestamp
# TAKES TIME
responses <- pmap_df(dayframe,\(days,old_url,new_url){
  my_timestamp <- paste0(format(days,'%Y%m%d'),"000000")
  monthly_timestamp_old <- paste0(base_url,extension[1],"&timestamp=",my_timestamp)
  monthly_timestamp_new <- paste0(base_url,extension[2],"&timestamp=",my_timestamp)
  response_old <- rawToChar(polite_GET(monthly_timestamp_old)$content)
  response_new <- rawToChar(polite_GET(monthly_timestamp_new)$content)
  data.frame(
    day = days,
    # we need to do this all messy like because 
    old = response_old,
    new = response_new
  )
})

save(responses, file="Final Project/Initial Responses.Robj")
load("Final Project/Initial Responses.Robj")

# extracting the timestamp from the json responses
unwrapped_responses <- responses |>
  pmap_df(\(day,old,new){
    unwrapped_old <- rjson::fromJSON(old)$archived_snapshots
    unwrapped_new <- rjson::fromJSON(new)$archived_snapshots
    
    data.frame(
      day = day,
      old_timestamp = if (is.null(unwrapped_old$closest)) NA else unwrapped_old$closest$timestamp,
      new_timestamp = if (is.null(unwrapped_new$closest)) NA else unwrapped_new$closest$timestamp
    )
  })

# doing some type conversion, then we have to split the data frame in half
# so we can perform the selects on the halves
# this should work, but the select is not working for some reason
timed_responses <- unwrapped_responses |>
  mutate(
    time_old_timestamp = as.POSIXct(old_timestamp,format='%Y%m%d'),
    time_new_timestamp = as.POSIXct(new_timestamp,format='%Y%m%d')
  ) |>
  select(months(date)==months(time_old_timestamp) |
           months(date)==months(time_new_timestamp))
  

# old_responses <- timed_responses |>
#   clean_names() |>
#   select(-is.na(old_timestamp)) |>
#   select(months(date)==months(time_old_timestamp)) 
         
         
#  select(date==time_new_timestamp)





