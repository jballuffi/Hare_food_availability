library(data.table)
library(ggplot2)


twigs <- fread("Input/salix_twigs.csv")

twigs[, sd(Weight), by = .(Class, Condition)]
twigs[, mean(Weight), by = .(Class, Condition)]

twigs[, sd(Weight), by = Condition]
twigs[, mean(Weight), by = Condition]

ggplot(twigs)+
  geom_boxplot(aes(y = Weight, x = Class, colour = Condition))+
  theme_minimal()

summary(lm(Weight ~ Class, twigs[Condition == "dry"]))

summary(lm(Weight ~ Class, twigs[Condition == "wet"]))


ggplot(twigs)+
  geom_boxplot(aes(y = Weight, x = Condition))+
  theme_minimal()

