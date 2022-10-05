library(ggplot2)
library(data.table)


source("Scripts/branch_to_twig.R")
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
sums <- biomass[, sum(Mass_total, na.rm = TRUE)/15, by = .(Species, Grid, Loc)]
setnames(sums, "V1", "Biomass")



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
#convert NAs to zeros
sums[is.na(Biomass), Biomass := 0]



#save as RDS file
saveRDS(sums, "Output/Data/transect_biomass.rds")
