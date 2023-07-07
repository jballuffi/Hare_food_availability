library(ggplot2)
library(data.table)


source("Scripts/01-allometric_eqn.R")
dat <- fread("Input/transects.csv")

#number of transects done
dat[, length(unique(Loc)), by = Grid]



# convert to biomass ------------------------------------------------------

#merge transect data with allometric equations
biomass <- merge(dat, eqn, by = "Species", all.x = TRUE)

#calculate biomass from branch BD for each height class
biomass[, Mass_low := (Slope*BD)*(Low/100)]
biomass[, Mass_med := (Slope*BD)*(Med/100)]
biomass[, Mass_high := (Slope*BD)*(High/100)]

#calculate biomass from branch BD for all height classes combined
biomass[, Mass_total := Slope*BD]

#replace is.na with zeros
biomass[is.na(Mass_low), Mass_low := 0]
biomass[is.na(Mass_med), Mass_med := 0]
biomass[is.na(Mass_high), Mass_high := 0]
biomass[is.na(Mass_total), Mass_total := 0]

#sum biomass per transect then divide by size of transect (15 m2)
#final unit is g/m2
sums <- biomass[, .(sum(Mass_low, na.rm = TRUE)/15, sum(Mass_med, na.rm = TRUE)/15, sum(Mass_high, na.rm = TRUE)/15, sum(Mass_total, na.rm = TRUE)/15), by = .(Species, Grid, Loc)]
names(sums) <- c("Species", "Grid", "Loc", "low", "med", "high", "total")



# fill in cases where there was no biomass of a species in a transect --------


#create empty sheet of transects and biomass for cases where there is zero biomass of a species
emptyspruce <- biomass[, unique(Loc), by = Grid]
setnames(emptyspruce, "V1", "Loc")
emptyspruce[, Species := "spruce"]

emptywillow <- biomass[, unique(Loc), by = Grid]
setnames(emptywillow, "V1", "Loc")
emptywillow[, Species := "willow"]

empty <- rbind(emptyspruce, emptywillow)

#merge empty sheet with sums sheet and NAs now appear occassionally in the biomass col
sums <- merge(empty, sums, by = c("Grid", "Loc", "Species"), all.x = TRUE)

#convert NAs to zeros. Musst be more efficient way!!!!!!!!!!!!!!!!
sums[is.na(low), low := 0][is.na(med), med := 0][is.na(high), high := 0][is.na(total), total := 0]



# get average biomass by grid -------------------------

#make data long form to work by height class
heights <- melt(sums, measure.vars = c("low", "med", "high"), variable.name = "Height", value.name = "Biomass")
heights[Height == "med", Height := "medium"]

#take avgerage biomass by species, height, and grid
# this will be exported to later be merged with nutrition and availability
avg <- heights[, .(mean(Biomass), median(Biomass), sd(Biomass)), by = .(Species, Height, Grid)]
names(avg) <- c("species", "height", "grid", "biomass_mean", "biomass_median", "biomass_sd")



# save outputs ------------------------------------------------------------

saveRDS(avg, "Output/Data/starting_biomass.rds")
