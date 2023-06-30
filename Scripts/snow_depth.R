
#script to collect snow data that was measured by the team every day on agnes, jo, and kloo
#and fill in later years with camera data


#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)




# collect data ------------------------------------------------------------

#list snow files from grid-level measurements and fread
snowfiles <- dir("data/", "Snow_grid*", full.names = TRUE) 
ls.snowfiles <- lapply(snowfiles, fread)
#rbindlist with an origin column
snowgrids <- rbindlist(ls.snowfiles, fill = TRUE, use.names = TRUE, idcol = "origin")
#now re-assign the origin column the file names
snowgrids[, origin := factor(origin, labels = basename(snowfiles))]


#data from camera traps
snowcams <- fread("data/Snow_cameras.csv")




# Create blank list of dates and grids ------------------------------------

#each data table is a list of dates for each grid
AG <- data.table(
    Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
    snowgrid = "Agnes")
KL <- data.table(
  Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
  snowgrid = "Kloo")
JO <- data.table(
  Date = seq.Date(as.Date("2015/02/10"), as.Date("2022/04/30"), "day"),
  snowgrid = "Jo")

#rbind all dates together
blanks <- rbind(AG, KL, JO)

#state months we care about (winter)
wintermonths <- c(11, 12, 1, 2, 3)

#subset to only those winter months
blanks <- blanks[month(Date) %in% wintermonths]



# clean up grid data -----------------------------------------------------------

#redo the grid names
snowgrids[grep("agnes", origin), snowgrid := "Agnes"]
snowgrids[grep("jo", origin), snowgrid := "Jo"]
snowgrids[grep("kloo", origin), snowgrid := "Kloo"]

#fix up other data cols
snowgrids[, COMMENTS := NULL]
snowgrids[, Date := lubridate::dmy(DATE)]
setnames(snowgrids, "OPEN SD", "SD")

#cut to just three columns of interest
gsnow <- snowgrids[, .(Date, snowgrid, SD)]
gsnow[, source := "ground"]
gsnow<-gsnow[!is.na(SD)] #remove no data from grids


# clean up camera trap data --------------------------------------------------

snowcams[, Date := tstrsplit(date_detected, " ", keep = 1)]
snowcams[, Date := mdy(Date)]

csnow <- snowcams[, mean(snow_depth_cm, na.rm = TRUE), by = .(Date, snowgrid)]
setnames(csnow, "V1", "SD")
csnow[, source := "camera"]



# merge data sheets -------------------------------------------------------

#bind camera data to human data
snow <- rbind(gsnow, csnow)

#merge with the blank list of dates so we can fill in NA dates
snowfull <- merge(blanks, snow, by = c("Date", "snowgrid"), all = TRUE)

#Order by grid and then date
setorder(snowfull, snowgrid, Date)

#add winter column
#categorize fixes into winters
snowfull[month(Date) > 10, winter := paste0(year(Date), "-", year(Date) + 1)]
snowfull[month(Date) < 4, winter := paste0(year(Date) - 1, "-", year(Date))]

#grab only winter
snowfull <- snowfull[!is.na(winter)]

#if source is NA say "fill"
snowfull[is.na(source), source := "fill"]

#see what major chunks of grids/winters are missing
snowfull[is.na(SD), .N, by = .(snowgrid, winter)]

#fill in missing snow depths with the last value (calls backwards in time)
snowfull[, SD := nafill(SD, "locf"), by = c("snowgrid", "winter")]

#in november of 2018, make snow depth 0.
#this month was empty but there was very little snow at the start of that december
snowfull[month(Date) == 11 & winter == "2018-2019", SD := 0]



# figure and save ---------------------------------------------------------

#figure to show snow depth over winter by year and location
(splot <- ggplot(snowfull)+
  geom_point(aes(x = Date, y = SD, color = snowgrid))+
  facet_wrap(~winter, scales = "free"))

#save data
saveRDS(snowfull, "data/snowgrids.rds")

#save figure
ggsave("Output/figures/snowdepth_summary.jpeg", splot, width = 11, height = 8.5, units = "in")

