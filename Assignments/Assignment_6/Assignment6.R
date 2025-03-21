library(tidyverse)
library(gganimate)

rdata <- read_csv("Data/BioLog_Plate_Data.csv")

data1 <- rdata |> janitor::clean_names()

sparkling_names <- names(data1) |>
  str_replace("hr_","")

names(data1) = sparkling_names

cdata <- data1 |>
  pivot_longer(cols = c("24","48","144"),
               names_to = "hour", values_to = "absorbance")|>
  mutate(hour = as.numeric(hour),
         absorbance = as.numeric(absorbance),
         sample_type = case_when(
           sample_id == "Clear_Creek" ~ "water",
           sample_id == "Waste_Water" ~ "water",
           sample_id == "Soil_1" ~ "soil",
           sample_id == "Soil_2" ~ "soil"
         ))

skimr::skim(cdata)

cdata |>
  filter(dilution == 0.1) |>
  ggplot(aes(y=absorbance,x=hour,color=sample_type))+
  geom_line()+
  facet_wrap(~substrate)

cdata |>
  filter(substrate=="Itaconic Acid")|>
  group_by(rep,hour,sample_id,well,dilution)|>
  summarise(mean=mean(absorbance))|>
  ggplot(aes(x=hour,y=mean,color=sample_id))+
  geom_line()+
  facet_wrap(~dilution)+
  gganimate::transition_reveal(hour)
  