
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)


# read in all cleaned and prepped data ------------------------------------

biomass <- readRDS("Output/Data/starting_biomass.rds")
nuts <- readRDS("Output/Data/starting_nutrition.rds")
willowgrid <- readRDS("Output/Data/starting_willow_avail_bygrid.rds")
willowsite <- readRDS("Output/Data/starting_willow_avail_bysite.rds")



# For now we are just merging together willow -----------------------------

biomass <- biomass[species == "willow"][, species := NULL]
nuts <- nuts[species == "willow"][, species := NULL]

#### WHERE IS CAMERA LOCATION. PROBABLY NEED TO CALCULATE WITH THIS FIRST?

# get total available biomass by day and grid -------------------------------------

#cases where proportion available was greater than one
willowgrid[propavail > 1, propavail := 1]

#merge starting grid biomass data with twig count data
willow <- merge(willowgrid, biomass, by = c("grid", "height"), all.x = TRUE)

#calculate available willow biomass
willow[, biomassavail := biomass_mean*propavail]



# get avg composition of willow available ---------------------------

#sum all biomass available across all three heights by day
willow[, biomassavail_allheight := sum(biomassavail), by = .(grid, date)]

#use the total biomass available to calculate the proportion of biomass each height contributes any day
willow[, biomassprop := biomassavail/biomassavail_allheight]

#merge in nutrition data
willow <- merge(willow, nuts, by = "height", all.x = TRUE)

#multiply composition by proportion available
willow[, DMDxbiomassprop := biomassprop*DMD_median]

#sum biomass across all heights by day and calculate average compositions
foodavail <- willow[, .(mean(temp), mean(snowdepth), mean(moon), sum(biomassavail), sum(DMDxbiomassprop)), by = .(grid, date)]
names(foodavail) <- c("grid", "date", "temp", "snowdepth", "moon", "biomassavail", "DMDavail")



# save  -------------------------------------------------------------------

#why is there a grid that is NA
foodavail <- foodavail[!is.na(grid)]

saveRDS(foodavail, "Output/Data/Total_daily_food_availability.rds")
