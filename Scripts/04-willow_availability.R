
library(data.table)
library(lubridate)
library(ggplot2)


# Read in data ------------------------------------------------------------

#list all cam trap data
camfiles <- list.files(path = "Input/Camera_traps/", full.names = TRUE)

#read in files
camdat <- lapply(camfiles, fread)

#function to clean up data frames
cleandat <- function(X){
  colnames(X) <- as.character(X[2,])
  x <- tail(X, -2)
  return(x)
}
camdat <- lapply(camdat, cleandat)

#rbind all dataframes
cams <- rbindlist(camdat, use.names = FALSE)



# clean data  -------------------------------------------------------------

#take only timer photos
cams <- cams[Trigger == "T"]

#what are the locations
cams[, unique(Location)]

#fix issue with names of locations, replace space with underscores
cams[, Location := gsub(" ", "_", Location)]

#create dates
cams[, Date := tstrsplit(`Image Name`, " ", keep = 1)]
cams[, Date := ymd(Date)]

#check dates
cams[, min(Date), Location]

#fix issue with date on one camera
cams[Location == "KL_48", Date := Date + 365]

#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/starting_flag_count.csv")



# load function  ---------------------------------------------------------------

getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# get twig availability by height ---------------------------------------------------

#remove extra underscore from loc
flags[, loc := gsub("_", "", loc)]

#make new full loc column
flags[, Location := paste0(grid, "_", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = "Location", all.x = TRUE)
setnames(twigs, "4_snow", "snowdepth")
setnames(twigs, "Moon Phase", "moon")

#copy twigs at this stage for stan's figures
twigstan <- twigs

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




# for stan ----------------------------------------------------------------

themepoints <- theme(axis.title = element_text(size=13),
                     axis.text = element_text(size=10),
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(linewidth = .5),
                     axis.line.y.left = element_line(size=.5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(size = 0.5, color = "grey90"))

head(twigstan)

twigstan[, avail := as.integer(`1_orange`) + as.integer(`2_yellow`) + as.integer(`3_pink`)]
twigstan[, total := orange + yellow + pink]
twigstan[, prop_avail := avail/total]

twigstan[, snowdepth := as.numeric(snowdepth)]

stan <- twigstan[, .(mean(snowdepth, na.rm = TRUE), mean(prop_avail, na.rm = TRUE)), Date]
names(stan) <- c("date", "snowdepth", "propavail")

prop_date <- 
ggplot(stan)+
  geom_path(aes(x = date, y = propavail, group = 1))+
  labs(y = "Proportion of twigs available", x = "Date")+
  themepoints

prop_snow <-
ggplot(stan)+
  geom_point(aes(x = snowdepth, y = propavail, group = 1))+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  themepoints




# save output data --------------------------------------------------------

#save the daily measure of avail across all grids
saveRDS(availavg, "Output/Data/starting_willow_avail_bygrid.rds")

#save the daily measures of avail by camera trap site
saveRDS(heights, "Output/Data/starting_willow_avail_bysite.rds")

ggsave("Output/Figures/stan_proportion_date.jpeg", prop_date, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/stan_proportion_snow.jpeg", prop_snow, width = 6, height = 4, unit = "in")
