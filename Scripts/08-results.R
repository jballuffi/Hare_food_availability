
library(data.table)
library(ggplot2)
library(lme4)




# import data -------------------------------------------------------------

food <- readRDS("Output/Data/Total_daily_food_availability.rds")
weights <- readRDS("Output/Data/weight_data.rds")
dat <- readRDS("Output/Data/Full_data_behandfood.rds")

#remove any days of not moving greater than 80,000 sec_axis
dat <- dat[notmoving < 80000]


# investigate food availability trends ------------------------------------

#no correlation between biomass and DMD available
cor(food$biomassavail, food$DMDavail)

#snow depth and DMD
ggplot(food)+
  geom_point(aes(y = DMDavail, x = snowdepth))+
  theme_minimal()

#snow depth and biomass
ggplot(food)+
  geom_point(aes(y = biomassavail, x = snowdepth))+
  theme_minimal()


#test how well we can predict DMD availability
summary(lm(DMDavail ~ temp + snowdepth, food))

#test how well we can predict biomass availability 
summary(lm(biomassavail ~ temp + snowdepth, food))



# investigate foraging trends ---------------------------------------------

#general trend over time
ggplot(dat)+
  geom_point(aes(x = date, y = foraging))+
  geom_path(aes(x = date, y = foraging, group = ID))


ggplot(dat)+
  geom_point(aes(x = temp, y = foraging, color = grid))

ggplot(weights)+
  geom_point(aes(x = DMDavail, y = weight, color = grid))


summary(glmer(foraging ~ moon + temp + DMDavail + biomassavail + (1|ID), dat))


summary(glmer(weight ~ temp + DMDavail + biomassavail + (1|ID), weights))

