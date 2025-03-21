library(MASS)
library(tidyverse)
library(easystats)
library(patchwork)

rdata <- read.csv("unicef-u5mr.csv")

data1 <- rdata |> janitor::clean_names()

sparkling_names <- names(data1) |>
  str_replace("u5mr_","")

names(data1) = sparkling_names

years <- data1 |>
  select(-country_name,-continent,-region)|>
  names()

cdata <- data1 |>
  pivot_longer(names_to = "year", values_to = "U5MR" ,cols=years)|>
  mutate(year=as.numeric(year))

p1 <- cdata |>
  na.omit() |>
  ggplot(aes(x=year,y=U5MR,group=country_name)) +
  geom_path()+
  facet_wrap(~continent)

ggsave("MACARTHUR_Plot_1.png",plot = p1)


p2 <- cdata |>
  na.omit() |>
  group_by(continent,year) |>
  summarise(Mean_U5MR=mean(U5MR)) |>
  ggplot(aes(x=year,y=Mean_U5MR,color=continent))+
  geom_path()

ggsave("MACARTHUR_Plot_2.png",plot = p2)

mod1 <- glm(formula = U5MR ~ year, data = cdata)
mod2 <- glm(U5MR ~ year + continent, data = cdata)
mod3 <- glm(U5MR ~ year * continent, data = cdata)

compare_performance(mod1,mod2,mod3)
compare_performance(mod1,mod2,mod3) %>% plot
# based upon this analysis, mod3 seems to give the best results

pdata <- cdata |>
  na.omit() |>
  mutate(mod1 = predict(mod1),
         mod2 = predict(mod2),
         mod3 = predict(mod3))

p3 <- pdata |>
  pivot_longer(cols=c("mod1","mod2","mod3"),names_to = "model",values_to = "pred")|>
  ggplot(aes(y=pred,x=year,color=continent))+
  geom_path() +
  facet_wrap(~model)

pf <- p1 + p2 / p3

pf

ggsave("MACARTHUR_composed_plots.png",pf)

test = data.frame(
  continent = c("Americas"),
  year = c(2020),
  country_name = c("Ecuador"),
  real = 13
)
test <- test |>
  mutate(pred1 = predict(mod3,test))

mod4 <- glm(U5MR ~ ., data = cdata)

step<-stepAIC(mod4)
step$formula

test <- test |>
  mutate(pred2 = predict(step,test))