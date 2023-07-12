
library(data.table)
library(ggplot2)
library(lme4)
library(ggpubr)



# import data -------------------------------------------------------------

food <- readRDS("Output/Data/Total_daily_food_availability.rds")
weights <- readRDS("Output/Data/weight_data.rds")
dat <- readRDS("Output/Data/Full_data_behandfood.rds")

#remove any days of not moving greater than 80,000 sec_axis
dat <- dat[notmoving < 80000]



# Make figure showing food trends over time -------------------------------

maxdate <- max(dat$date)
mindate <- min(dat$date)

food2 <- food[date > mindate & date < maxdate]

(biomassplot <- 
   ggplot(food2)+
   geom_path(aes(x = date, y = biomassavail, color = grid, group = grid))+
   labs(y = "Available biomass (g/m2", x = "Date")+
   theme_minimal())

(DMDplot <- 
    ggplot(food2)+
    geom_path(aes(x = date, y = DMDavail*100, color = grid, group = grid))+
    labs(y = "Average available DMD (%)", x = "Date")+
    theme_minimal())

foodplot <- ggarrange(biomassplot, DMDplot, ncol = 1, nrow = 2)



# investigate foraging trends ---------------------------------------------

#general trend over time
(foragetrend <- 
  ggplot(dat)+
  geom_point(aes(x = date, y = foraging/3600, color = grid))+
  geom_path(aes(x = date, y = foraging/3600, group = ID, color = grid))+
  labs(y = "Foraging rate (hrs/day)", x = "Date")+
  theme_minimal())

foragemod <- glmer(foraging/3600 ~ moon + temp + DMDavail + biomassavail + (1|ID), dat)


(foragetemp <- ggplot(dat)+
  geom_point(aes(x = temp, y = foraging/3600))+
  labs(y = "Foraging rate (hrs/day)", x = "Temperature (C)")+
  theme_minimal())

(foragebiomass <- ggplot(dat)+
  geom_point(aes(x = biomassavail, y = foraging/3600))+
  labs(y = "Foraging rate (hrs/day)", x = "Available bimoass (g/m2)")+
  theme_minimal())



# save --------------------------------------------------------------------

ggsave("Output/Figures/Food_availability_over_winter.jpeg", foodplot, width = 8, height = 9, units = "in")
ggsave("Output/Figures/Forage_over_winter.jpeg", foragetrend, width = 6, height = 4, units = "in")
ggsave("Output/Figures/Forage_temperature.jpeg", foragetemp, width = 6, height = 4, units = "in")
ggsave("Output/Figures/Forage_biomass.jpeg", foragebiomass, width = 6, height = 4, units = "in")
