library(MASS)

library(tidyverse)
library(easystats)
library(caret)
library(palmerpenguins)

rdata <- penguins |>
  dplyr::filter(!is.na(sex))|>
  mutate(sex.male = sex=="male")
  

mod1 <- glm(formula = sex.male ~ .,
            data = rdata |> dplyr::select(-sex),
            family="binomial")
summary(mod1)

pdata <- rdata |>
  mutate(pred = predict(mod1,rdata,type="response"))

pdata |>
  ggplot(aes(x=body_mass_g,y=pred,color = sex))+
  geom_point()

pdata <- pdata |>
  mutate(error = pred >= .5) |>
  mutate(success = error == sex.male)

pdata$success |> summary()

rdata2 <- read_csv("./Data/GradSchool_Admissions.csv")

rdata2 <- rdata2 |>
  dplyr::filter(is.na())

mod2 <- glm(formula=admit ~ .,
            data = rdata2,
            family = "binomial")

pdata2 <- rdata2 |>
  mutate(pred = predict(mod2,rdata2,type="response"))

pdata2 |>
  ggplot(aes(x=gpa, y=pred,color=factor(rank)))+
  geom_smooth()+
  geom_point()

report(mod2)

pdata2 <- pdata2 |>

