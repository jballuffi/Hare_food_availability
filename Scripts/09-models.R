

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

#read in cleaned and prepped data made in script 8
dt <- readRDS("Output/Data/Total_daily_food_availability.rds")

dt[, gridloc := paste0(grid, "_", loc)]




# explore final data ------------------------------------------------------

ggplot(dt)+
  geom_point(aes(x = snow, y = biomass, color = grid))

# highbios <- dt[biomass > 40, unique(gridloc)]
# 
# highbiomass <- dt[gridloc %in% highbios]
# highbiomass[, unique(gridloc), winter]
# 
# ggplot(highbiomass)+
#   geom_point(aes(x = snow, y = biomass, color = gridloc))

ggplot(dt)+
  geom_point(aes(x = snow, y = proportion, color = grid))

ggplot(dt)+
  geom_point(aes(x = snow, y = CP_comp))



# make models -------------------------------------------------------------

bs <- lm(biomass ~ snow, dt)


bs <- dt[, modout(yvar = biomass, xvar1 = snow)]

cps <- dt[, modout(yvar = CP_comp, xvar1 = snow)]

ps <- dt[, modout(yvar = proportion, xvar1 = snow)]


