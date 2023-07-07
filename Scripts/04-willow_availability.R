
library(data.table)
library(lubridate)
library(ggplot2)


# Read in data ------------------------------------------------------------

#list all cam trap data
camfiles <- list.files(path = "Input/Camera_traps/", full.names = TRUE)

#read in files and rbind
cams <- lapply(camfiles, fread)
cams <- rbindlist(cams, use.names = TRUE)

#set date as date
cams[, Date := mdy(Date)]

#fix issue with date on one camera
cams[Location == "KL 48", Date := Date + 365]

#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/camera_flag_count.csv")



# get twig availability by height ---------------------------------------------------

#make new full loc column
flags[, Location := paste0(grid, " ", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = "Location", all.x = TRUE)
setnames(twigs, "4_snow", "snowdepth")

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := `1_orange`/orange]
twigs[, medium := `2_yellow`/yellow]
twigs[, high := `3_pink`/pink]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, Date, snowdepth, Temp, grid, loc, low, medium, high)]

#melt twig availability by height class
heights <- melt(twigs, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail")

#convert farenheit to celcius
heights[, temp := (Temp-32)/1.8][, Temp := NULL]

#change some col names
setnames(heights, c("Date", "Location"), c("date", "location"))

# collect all stats for twig availability ---------------------------------

#collect avg willow twig availability by grid, date, and height, along with snow and temp data
availavg <- heights[, .(mean(snowdepth), mean(temp), mean(propavail)), by = .(grid, date, height)]
names(availavg) <- c("grid", "date", "height", "snowdepth", "temp", "propavail")



# save output data --------------------------------------------------------

#save the daily measure of avail across all grids
saveRDS(availavg, "Output/Data/starting_willow_avail_bygrid.rds")

#save the daily measures of avail by camera trap site
saveRDS(heights, "Output/Data/starting_willow_avail_bysite.rds")
