
#script to clean camera trap data and prepare for future merges

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


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

#rename cols
setnames(cams, "Moon Phase", "Moon")

#make snowdepth integer
cams[, Snow := as.integer(`4_snow`)]


# convert moon phases to levels of illumination ---------------------------

#convert to a proportion of illumination
cams[grep("Quarter", Moon), Moon := .5]
cams[grep("New", Moon), Moon := 0]
cams[grep("Full", Moon), Moon := 1]
cams[grep("Crescent", Moon), Moon := .25]
cams[grep("Gibbous", Moon), Moon := .75]

#set as numeric, capitalize 
cams[, Moon := as.numeric(Moon)]




# subset to key cols ------------------------------------------------------

camtwigs <- cams[, .(Location, Date, Time, Moon, Temp, Snow, `1_orange`, `2_yellow`, `3_pink`)]



# save --------------------------------------------------------------------

saveRDS(camtwigs, "Output/Data/camtraps.rds")

