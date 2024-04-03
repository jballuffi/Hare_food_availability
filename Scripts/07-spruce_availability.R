

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load data for starting biomass and camera traps
biomass <- readRDS("Output/Data/starting_biomass.rds") #WHY IS MEDIAN ZERO
willow <- readRDS("Output/Data/willow_avail_bysite.rds")


# subset data -------------------------------------------------------------

#take only spruce from biomass data
spruce <- biomass[species == "spruce"]

#reorder willow data by date
willow <- willow[order(Date)]

#check snow depth data with a simple figure
ggplot(willow)+
    geom_path(aes(x = Date, y = Snow, color = grid, group = grid))



# create a guide from snow depth to hare reach -------------------------

#hares can reach 50 cm above snow
hreach <- 50

#create snow depths from 0 cm to 100 cm
reach <- data.table(snow = seq(0:100)) 

#calculate how high a hare can reach based on snow depth
reach[, maxreach := snow + hreach]
reach[, minreach := snow]

#create columns for prop avail high, med, and low

            