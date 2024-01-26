
library(data.table)
library(ggplot2)
library(lme4)
library(ggpubr)



# import data -------------------------------------------------------------

food <- readRDS("Output/Data/Total_daily_food_availability.rds")
weights <- readRDS("Output/Data/weight_data.rds")
dat <- readRDS("Output/Data/Full_data_behandfood.rds")
will <- readRDS("Output/Data/starting_willow_avail_bysite.rds")


#remove any days of not moving greater than 80,000 sec_axis
dat <- dat[notmoving < 80000]

themepoints <- theme(axis.title = element_text(size=13),
                     axis.text = element_text(size=10),
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(linewidth = .5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))

maxdate <- max(dat$date)
mindate <- min(dat$date)



# for stan ----------------------------------------------------------------

allgrid <- food[, .(mean(snowdepth, na.rm = TRUE), mean(biomassavail, na.rm = TRUE), mean(DMDavail, na.rm = TRUE)), date]
names(allgrid) <- c("date", "snowdepth", "biomassavail", "DMDavail")

allgrid2 <- allgrid[date > mindate & date < maxdate]

stan_date <- 
  ggplot(allgrid)+
  geom_path(aes(x = date, y = biomassavail, group = 1))+
  labs(y = "Available biomass (g/m2)", x = "Date")+
  themepoints

  ggplot(allgrid2)+
  geom_path(aes(x = date, y = biomassavail, group = 1))+
  labs(y = "Available biomass (g/m2)", x = "Date")+
  themepoints

stan_snow <- 
  ggplot(allgrid)+
  geom_point(aes(x = snowdepth, y = biomassavail, group = 1))+
  labs(y = "Available biomass (g/m2)", x = "Snow depth (cm)")+
  themepoints

stan_snow_cropped <- 
  ggplot(allgrid2)+
  geom_point(aes(x = snowdepth, y = biomassavail, group = 1))+
  labs(y = "Available biomass (g/m2)", x = "Snowdepth ")+
  themepoints


twig_all <- 



# Make figure showing food trends over time -------------------------------



food2 <- food[date > mindate & date < maxdate]


(biomassplot <- 
   ggplot(food2)+
   geom_path(aes(x = date, y = biomassavail, color = grid, group = grid))+
   labs(y = "Available biomass (g/m2)", x = "")+
   themepoints)

(DMDplot <- 
    ggplot(food2)+
    geom_path(aes(x = date, y = DMDavail*100, color = grid, group = grid))+
    labs(y = "Average available DMD (%)", x = "Date")+
    themepoints)

foodplot <- ggarrange(biomassplot, DMDplot, ncol = 1, nrow = 2)



# food trends in response to snow -----------------------------------------

food[, m :=  as.factor(month(date))]

snowplotbiomass <-
   ggplot(food)+
   geom_point(aes(x = snowdepth, y = biomassavail, color = grid))+
   labs(y = "Available biomass (g/m2)", x = "")+
   themepoints

snowplotDMD <-
  ggplot(food)+
  geom_point(aes(x = snowdepth, y = DMDavail*100, color = grid))+
  labs(y = "Available average DMD (%)", x = "Snow depth (cm)")+
  themepoints

snowplot <- ggarrange(snowplotbiomass, snowplotDMD, ncol = 1, nrow = 2)


# investigate foraging trends ---------------------------------------------

#general trend over time
(foragetrend <- 
  ggplot(dat)+
  geom_point(aes(x = date, y = foraging/3600, ))+
  geom_path(aes(x = date, y = foraging/3600, group = ID))+
  labs(y = "Foraging rate (hrs/day)", x = "Date")+
  themepoints)

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
ggsave("Output/Figures/Food_availability_snow.jpeg", snowplot, width = 8, height = 9, units = "in")

ggsave("Output/Figures/Forage_over_winter.jpeg", foragetrend, width = 6, height = 5, units = "in")


ggsave("Output/Figures/Forage_temperature.jpeg", foragetemp, width = 6, height = 4, units = "in")
ggsave("Output/Figures/Forage_biomass.jpeg", foragebiomass, width = 6, height = 4, units = "in")


#save stan's figure
ggsave("Output/Figures/stan_biomass_snow.jpeg", stan_snow, width = 6, height = 4, units = "in")
ggsave("Output/Figures/stan_biomass_date.jpeg", stan_date, width = 6, height = 4, units = "in")
