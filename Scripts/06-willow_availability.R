
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

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := as.integer(`1_orange`)/orange]
twigs[, medium := as.integer(`2_yellow`)/yellow]
twigs[, high := as.integer(`3_pink`)/pink]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, Date, Snow, Temp, Moon, grid, loc, low, medium, high)]

#melt twig availability by height class
heights <- data.table::melt(twigs, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail")


#convert farenheit to celcius
heights[, Temp := as.integer(Temp)]
heights[, temp := (Temp-32)/1.8]



# collect all stats for twig availability ---------------------------------

#collect avg willow twig availability by grid, date, and height, along with snow and temp data
availavg <- heights[, .(snow = mean(Snow), temp = mean(temp), moon = mean(Moon), propavail = mean(propavail)), by = .(grid, Date, height)]




# save output data --------------------------------------------------------

#save the daily measure of avail across all grids
saveRDS(availavg, "Output/Data/starting_willow_avail_bygrid.rds")

#save the daily measures of avail by camera trap site
saveRDS(heights, "Output/Data/starting_willow_avail_bysite.rds")

