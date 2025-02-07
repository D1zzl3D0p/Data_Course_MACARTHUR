# SETUP ####

## load packages ####
library(tidyverse)
library(ggplot2)

## load data ####
dat <- read_delim("./Data/DatasaurusDozen.tsv")
View(dat)
str(dat)
unique(dat$dataset)
dat |>
  filter(dataset=="dino")|>
    ggplot(aes(x,y))+
      geom_point()

t_dat = dat[dat$dataset=="dino",]
plot(t_dat$x,t_dat$y)
