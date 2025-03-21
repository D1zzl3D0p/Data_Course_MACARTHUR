library(tidyverse)
library(janitor)

rdata <- read.csv("./Data/Utah_Religions_by_County.csv")

View(rdata)

cdata<-rdata |> 
  clean_names()|>
  pivot_longer(names_to = "religion", values_to = "proportion", cols=columns)|>
  select(-religious)

columns <- cdata |>
  select(-pop_2010,-county,-religious,)|>
  names()

ordering <- cdata |>
  group_by(religion)|>
  summarise(sum = sum(proportion))|>
  arrange(desc(sum))
  

final <- cdata |>
  clean_names() |>
  # pivot_longer(names_to = "religion", values_to = "proportion", cols=columns)|>
  mutate(religion=factor(religion,levels = ordering$religion),
         proportion=as.numeric(proportion))|>
  arrange(desc(proportion))

View(final)

final |>
  ggplot(aes(x=religion,y=proportion))+
  geom_col()+
  facet_wrap(~county)+
  theme(
    axis.text.x = element_text(angle=90)
  )
