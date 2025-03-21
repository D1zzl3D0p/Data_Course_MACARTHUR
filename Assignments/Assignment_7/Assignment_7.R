library(tidyverse)
library(janitor)

rdata <- read.csv("Data/Utah_Religions_by_County.csv")

View(rdata)

data1 <- rdata |> clean_names()

columns <- data1 |>
  select(-pop_2010,-county,-religious,)|>
  names()

cdata<-data1 |> 
  pivot_longer(names_to = "religion", values_to = "proportion", cols=columns)|>
  select(-religious)

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

mod <- glm(pop_2010 ~ proportion:religion,data=final)

summary(mod)

# “Does population of a county correlate with the proportion of any specific religious group in that county?”
# according to the above analysis, yes, LDS and Muslim religions both correlate with population

cor(rdata$Non.Religious,rdata$Assemblies.of.God)
cor(rdata$Non.Religious,rdata$Episcopal.Church)
cor(rdata$Non.Religious,rdata$Pentecostal.Church.of.God)
cor(rdata$Non.Religious,rdata$Greek.Orthodox)
cor(rdata$Non.Religious,rdata$LDS)
cor(rdata$Non.Religious,rdata$Southern.Baptist.Convention)
cor(rdata$Non.Religious,rdata$United.Methodist.Church)
cor(rdata$Non.Religious,rdata$Buddhism.Mahayana)
cor(rdata$Non.Religious,rdata$Catholic)
cor(rdata$Non.Religious,rdata$Evangelical)
cor(rdata$Non.Religious,rdata$Muslim)
cor(rdata$Non.Religious,rdata$Non.Denominational)
cor(rdata$Non.Religious,rdata$Orthodox)


summary(mod)

# “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
# Yes, generally higher proportions of LDS people correlate with smaller proportions of non-religious, the same mostly
# holds true for Episocopal church