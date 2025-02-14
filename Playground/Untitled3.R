library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggimage)
library(wesanderson)
library(GGally)

pal<-wesanderson::wes_palette("IsleofDogs1",3)

p <-
penguins|>
  filter(!is.na(sex)) |>
  mutate(sex = str_to_sentence(sex)) |>
  ggplot(aes(x=flipper_length_mm,y=body_mass_g,color=species))+
  geom_point()+
  stat_ellipse()+
  facet_wrap(~sex)+
  theme_bw() +
  labs(
    x="Flipper length (mm)",
    y="Body mass (g)"
  ) + 
  scale_color_manual(values = pal)

p

p + gganimate::transition_states(species) + gganimate::ease_aes()

penguins |>
  ggplot(aes(x=bill_length_mm,color=species,fill=species))+
  geom_density(alpha=0.25)

penguins |>
  GGally::ggpairs()

ggsave(p,filename = "./blah/blah/blah.png") #saves the file
