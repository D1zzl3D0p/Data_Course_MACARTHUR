library(tidyverse)
library(gganimate)
library(ggimage)
library(GGally)

rdata <- read_csv("Data/streaming_service.csv")
stream <- rdata |>
  # you have to use paste0 because default paste adds a space in between
  mutate(date=as.POSIXct(paste0("01-",date),format='%d-%b-%Y'))|> 
  mutate(service = as.factor(service))

stream |>
  arrange(date) |>
  ggplot(aes(x=date,y=price,color=service))+
  geom_point()+
  geom_line()
