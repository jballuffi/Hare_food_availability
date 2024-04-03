

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load data for starting biomass and camera traps
biomass <- readRDS("Output/Data/starting_biomass.rds") #WHY IS MEDIAN ZERO
cams <- readRDS("Output/Data/camtraps.rds")



# subset data -------------------------------------------------------------

#take only spruce from biomass data
spruce <- biomass[species == "spruce"]

#create grid column in cams 
cams[, Grid := tstrsplit(Location, "_", keep = 1)]

#take average snow depth by grid and date from camera trap data
snow <- cams[, .(mean = mean(Snow, na.rm = TRUE), median = median(Snow, na.rm = TRUE), sd = sd(Snow, na.rm = TRUE)), by = .(Grid, Date)]

#reorder snow data by date
snow <- snow[order(Date)]

#check snow depth data with a simple figure
ggplot(snow)+
    geom_path(aes(x = Date, y = median, color = Grid, group = Grid))



# create a guide from snow depth to hare reach -------------------------

#hares can reach 50 cm above snow
hreach <- 50

#create snow depths from 0 cm to 100 cm
reach <- data.table(snow = seq(0:100)) 

#calculate how high a hare can reach based on snow depth
reach[, maxreach := snow + hreach]
reach[, minreach := snow]

#create columns for prop avail high, med, and low

            