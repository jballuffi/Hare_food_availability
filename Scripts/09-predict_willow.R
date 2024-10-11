

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

#read in cleaned and prepped data made in script 8
dt <- readRDS("Output/Data/Total_daily_food_availability.rds")

dt[, gridloc := paste0(grid, "_", loc)]



# manipulate data ---------------------------------------------------------

means <- dt[, .(biomass = mean(biomass, na.rm = TRUE), 
                CP_grams = mean(CP_grams, na.rm = TRUE), 
                CP_comp = mean(CP_comp, na.rm = TRUE),
                snow = mean(snow, na.rm = TRUE), 
                temp = mean(temp, na.rm = TRUE)),
            by = .(winter, grid, species, idate)]

means <- means[order(idate, grid)]


#make just willow data
willow <- dt[species == "willow"]
willow <- willow[order(grid, loc, idate)]



# explore final data ------------------------------------------------------

#trend over time

ggplot(means)+
  geom_path(aes(x = idate, y = biomass, group = grid, color = grid))+
  facet_wrap(~winter + species, scale = "free")+
  theme_minimal()

ggplot(means)+
  geom_path(aes(x = idate, y = CP_comp, group = grid, color = grid))+
  facet_wrap(~winter + species, scale = "free")+
  theme_minimal()



#trend over snow depth

ggplot(dt)+
  geom_point(aes(x = snow, y = biomass, color = grid))+
  facet_wrap(~winter + species, scale = "free")

ggplot(dt)+
  geom_point(aes(x = snow, y = CP_comp, color = grid))+
  facet_wrap(~species, scale = "free")






# predict willow availability -------------------------------------------------------------



bs <- lm(biomass ~ snow, willow)
bsg <- lm(biomass ~ snow + grid, willow)
bsgt <- lm(biomass ~ snow + grid + temp, willow)


bs <- dt[, modout(yvar = biomass, xvar1 = snow)]

cps <- dt[, modout(yvar = CP_comp, xvar1 = snow)]

ps <- dt[, modout(yvar = proportion, xvar1 = snow)]


