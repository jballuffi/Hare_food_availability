
library(data.table)
library(lubridate)
library(ggplot2)


# Read in data ------------------------------------------------------------

#list all cam trap data
camfiles <- list.files(path = "Input/Camera_traps/", full.names = TRUE)

#read in files
camdat <- lapply(camfiles, fread)

#function to clean up dataframes
cleandat <- function(X){
  colnames(X) <- as.character(X[2,])
  x <- tail(X, -2)
  return(x)
}

camdat <- lapply(camdat, cleandat)

cams <- rbindlist(camdat, use.names = FALSE)



#set date as date
cams[, Date := mdy(Date)]

#fix issue with date on one camera
cams[Location == "KL 48", Date := Date + 365]

#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/camera_flag_count.csv")



# load function  ---------------------------------------------------------------

getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# get twig availability by height ---------------------------------------------------

#make new full loc column
flags[, Location := paste0(grid, " ", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = "Location", all.x = TRUE)
setnames(twigs, "4_snow", "snowdepth")
setnames(twigs, "Moon Phase", "moon")

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := as.integer(`1_orange`)/orange]
twigs[, medium := as.integer(`2_yellow`)/yellow]
twigs[, high := as.integer(`3_pink`)/pink]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, Date, snowdepth, Temp, moon, grid, loc, low, medium, high)]

#melt twig availability by height class
heights <- melt(twigs, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail")

#convert farenheit to celcius
heights[, Temp := as.integer(Temp)]
heights[, temp := (Temp-32)/1.8][, Temp := NULL]

#change some col names
setnames(heights, c("Date", "Location"), c("date", "location"))

# convert moon phases to levels of illumination ---------------------------

#convert to a proportion of illumination
heights[grep("Quarter", moon), moon := .5]
heights[grep("New", moon), moon := 0]
heights[grep("Full", moon), moon := 1]
heights[grep("Crescent", moon), moon := .25]
heights[grep("Gibbous", moon), moon := .75]

#set as numeric
heights[, moon := as.numeric(moon)]
heights[, snowdepth := as.integer(snowdepth)]

# collect all stats for twig availability ---------------------------------

#collect avg willow twig availability by grid, date, and height, along with snow and temp data
availavg <- heights[, .(mean(snowdepth), mean(temp), mean(moon), mean(propavail)), by = .(grid, date, height)]
names(availavg) <- c("grid", "date", "height", "snowdepth", "temp", "moon", "propavail")





# save output data --------------------------------------------------------

#save the daily measure of avail across all grids
saveRDS(availavg, "Output/Data/starting_willow_avail_bygrid.rds")

#save the daily measures of avail by camera trap site
saveRDS(heights, "Output/Data/starting_willow_avail_bysite.rds")
