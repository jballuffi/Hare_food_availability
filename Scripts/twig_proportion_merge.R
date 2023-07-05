
library(data.table)
library(lubridate)
library(ggplot2)


# Read in data ------------------------------------------------------------

nuts <- readRDS("Output/Data/nutrient_biomass.rds")

camfiles <- list.files(path = "Input/Camera_traps/", full.names = TRUE)

cams <- lapply(camfiles, fread)
cams <- rbindlist(cams, use.names = TRUE)

camflags <- fread("Input/camera_flag_count.csv")



# get twig availability ---------------------------------------------------

#make new full loc column
camflags[, Location := paste0(grid, " ", loc)]

twigs <- merge(cams, camflags, by = "Location", all.x = TRUE)
setnames(twigs, "4_snow", "snowdepth")

twigs[, orangeprop := `1_orange`/orange]
twigs[, yellowprop := `2_yellow`/yellow]
twigs[, pinkprop := `3_pink`/pink]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, Date, snowdepth, Temp, grid, loc, orangeprop, yellowprop, pinkprop)]
