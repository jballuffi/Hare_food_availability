
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/starting_flag_count.csv")
cams <- readRDS("Output/Data/camtraps.rds")



# get twig availability by height ---------------------------------------------------

#remove extra underscore from loc
flags[, loc := gsub("_", "", loc)]

#make new full loc column
flags[, Location := paste0(grid, "_", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = "Location", all.x = TRUE)

twigs[, orangeC := as.integer(`1_orange`)][, yellowC := as.integer(`2_yellow`)][, pinkC := as.integer(`3_pink`)]

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := orangeC/orange]
twigs[, medium := yellowC/yellow]
twigs[, high := pinkC/pink]
twigs[, allheights := (orangeC + yellowC + pinkC)/(orange + yellow + pink)]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, Date, Snow, Temp, Moon, grid, loc, low, medium, high, allheights)]

#melt twig availability by height class
willow <- data.table::melt(twigs, measure.vars = c("low", "medium", "high", "allheights"), variable.name = "height", value.name = "propavail_willow")

#convert farenheit to celcius
willow[, Temp := as.integer(Temp)]
willow[, temp := (Temp-32)/1.8]

willow <- willow[order(Location, Date)]

willow[propavail_willow > 1, propavail_willow := 1]

# save output data --------------------------------------------------------

#save the daily measures of avail by camera trap site
saveRDS(willow, "Output/Data/willow_avail_bysite.rds")

