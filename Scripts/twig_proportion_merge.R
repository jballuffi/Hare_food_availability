
library(data.table)
library(lubridate)
library(ggplot2)


# Read in data ------------------------------------------------------------

nuts <- readRDS("Output/Data/nutrient_biomass.rds")

camfiles <- list.files(path = "Input/Camera_traps/", full.names = TRUE)

cams <- lapply(camfiles, fread)
cams <- rbindlist(cams, use.names = TRUE)

cams[, Date := mdy(Date)]
cams[Location == "KL 48", Date := Date + 365]

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

#rename flag colors to be height classes
setnames(twigs, c("orangeprop", "yellowprop", "pinkprop"), c("low", "medium", "high"))

#melt twig data
twigs <- melt(twigs, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail")



# get average biomass and nutrient biomass by grid for willow ------------------------

#calculate available CP and Biomass
nuts[, CPavail := Biomass*(CP/100)]
nuts[, NDFavail := Biomass*(NDF/100)]

#calculate total availability by height class, species, and grid
totals <- nuts[, .(mean(Biomass), mean(CPavail), mean(NDFavail)), by = .(Species, Height, Grid)]
names(totals) <- c("species", "height", "grid", "biomasstotal", "CPtotal", "NDFtotal")

#take just willow from nutrient availability
willow <- totals[species == "willow"]



# merge starting nutrient avail with twig avail ---------------------------

#merge total availability with proportions of twig available 
willowavail <- merge(twigs, willow, by = c("grid","height"), all.x = TRUE)

#now calc the actual availability of biomass, CP, and NDF with snow depth
willowavail[, biomassavail := biomasstotal*propavail]
willowavail[, CPavail := CPtotal*propavail]
willowavail[, NDFavail := NDFtotal*propavail]

ggplot(willowavail)+
  geom_path(aes(x = Date, y = biomassavail, group = height, color = height))


# sum all willow avail across height by day --------------------------------------

willowall <- willowavail[, .(mean(snowdepth), mean(Temp), sum(biomassavail), sum(CPavail), sum(NDFavail)), by = .(grid, Date)]
names(willowall) <- c("grid", "date", "snowd", "temp", "biomass", "CP", "NDF")


ggplot(willowall)+
  geom_path(aes(x = date, y = biomass, group = grid, color = grid))

