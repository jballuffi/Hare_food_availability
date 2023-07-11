
library(data.table)
library(ggplot2)




# import data -------------------------------------------------------------

food <- readRDS("Output/Data/Total_daily_food_availability.rds")

dat <- readRDS("Output/Data/Full_data_behandfood.rds")



# investigate food availability trends ------------------------------------

#no correlation between biomass and DMD available
ggplot(food)+
  geom_point(aes(y = DMDavail, x = biomassavail))+
  theme_minimal()
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

#remove any days of not moving greater than 80,000 sec_axis
dat <- dat[notmoving < 80000]

ggplot(dat)+
  geom_point(aes(y = foraging, x = date))+
  geom_path(aes(y = foraging, x = date, group = ID))

ggplot(dat)+
  geom_point(aes(x = snowdepth, y = foraging, color = grid))



