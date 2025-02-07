library(tidyverse)
library(palmerpenguins)
library(plotly)
library(ggimage)

plot <-
  penguins |> 
    na.omit() |>
      ggplot(aes(x=bill_length_mm,y=bill_depth_mm,color=species))+
      geom_point()+
      facet_wrap(~sex)

ggplotly(plot)

View(penguins |> na.omit())
View(penguins |> filter(!is.na(sex)))


# plot of male penguins only
# show body mass for each species

pen <-
  penguins |>
    mutate(pokemon = case_when(
      species == "Adelie" ~ "pikachu",
      species == "Chinstrap" ~ "kabuto",
      species == "Gentoo" ~ "porygon",
    ))

pen |>
  filter(sex=="male")|>
    ggplot(aes(y=body_mass_g,x=species))+
    #geom_point()+ 
    geom_violin()+
    #geom_pokemon(aes(image=pokemon))+
    geom_jitter(width = 0.1,alpha=.25)+
    theme_minimal()+
    theme(
      panel.grid.minor = element_line(linetype = 1221231)
    )+
    scale_color_viridis_d(option="inferno",end=.8,begin=.3)
