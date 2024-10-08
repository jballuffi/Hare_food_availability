
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in all cleaned and prepped data ------------------------------------

startingbiomass <- readRDS("Output/Data/starting_biomass.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition.rds")
prop <- readRDS("Output/Data/proportion_available.rds")

#change col names in starting nutrients
setnames(startingnuts, c("Species", "Height"), c("species", "height"))



# melt the proportion available by species --------------------------------

#melt by species
prop <- melt.data.table(prop, measure.vars = c("propavail_willow", "propavail_spruce"), 
                        variable.name = "species", 
                        value.name = "propavail")

#take only species name from species column
prop[, species := tstrsplit(species, "_", keep = 2)]



# merge in biomass and quality --------------------------------------------------------

#merge in biomass and proportion available
#the median biomass is zero for spruce at lower height classes. Probably should use means
biomass <- merge(prop, startingbiomass[, .(species, height, grid, biomass_mean)], 
                 by = c("grid", "species", "height"), all.x = TRUE)

#merge in quality to biomass and proportion available
daily <- merge(biomass, startingnuts[, .(species, height, CP_mean)], 
               by = c("species", "height"), all.x = TRUE)

#reorder daily values
daily <- daily[order(Location, Date, species)]

#cases where proportion available was greater than one
daily[propavail > 1, propavail := 1]

#remove the all heights row for now
daily <- daily[!height == "allheights"]



# get total available biomass by day  -------------------------------------

#calculate available biomass based on mean
daily[, biomassavail := biomass_mean*propavail]



# get avg composition of willow available ---------------------------

#sum all biomass available across all three heights by day
daily[, biomassavail_all := sum(biomassavail), by = .(grid, loc, Date, species)]

#calculate the proportion of biomass each height contributes each day
daily[, biomassprop := biomassavail/biomassavail_all]

#multiply composition by proportion available
daily[, CPxbiomassprop := biomassprop*CP_mean]  # I think this is wrong




# save  -------------------------------------------------------------------

#why is there a grid that is NA
#foodavail <- foodavail[!is.na(grid)]

saveRDS(foodavail, "Output/Data/Total_daily_food_availability.rds")
