library(dplyr)
library(mice)
library(ggplot2)
library(betareg)

data <- read.csv(file.choose(), sep = ",", header = TRUE)

str(data)
md.pattern(data ) # ceck for missing data 
task <- data %>%
  group_by(bacteria, age) %>%
  summarise(
    tot = n(),
    res = sum(result == 1),
    pp = res / tot
  ) %>%
  ungroup()

ggplot(task, aes(x = age, y = pp, color = bacteria)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Age",
    y = "Proportion of Resistant Infections")


#simple models 

#model1 simple regression
mod1 <- glm(result ~ age, family = binomial(link = "logit"), data = data)
summary(mod1)
exp(coef(mod1))

mod2 <- glm(result ~ age + bacteria , family = binomial(link = "logit"), data = data)
summary(mod2)


mod3 <- glm(result ~ age*bacteria , family = binomial(link = "logit"), data = data)
summary(mod3)

exp(coef(mod3))

#model2  beta regression

moda <- betareg(proportion_resistant ~ age, data = resistance_summary)
summary(moda)

exp(coef(moda))








