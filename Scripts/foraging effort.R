#script that investigates foraging data from previous winters
#also includes snow depths from those years

library(data.table)
library(ggplot2)
library(lubridate)


beh <- fread("Input/allHareDailyValues2015_2021.csv")
trapping <- fread("Input/Trapping_data_all_records.csv")


#this function is from the R folder in the footload project
getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}





# Snow data ---------------------------------------------------------------

#list snow files and read in
snowfiles <- dir("Input/", "Snow_cam*", full.names = TRUE) 
ls.snowfiles <- lapply(snowfiles, fread)

#rbindlist with an origin column
snows <- rbindlist(ls.snowfiles, fill = TRUE, use.names = TRUE, idcol = "origin")
#now re-assign the origin column the file names
snows[, origin := factor(origin, labels = basename(snowfiles))]

#redo the grid names
snows[grep("agnes", origin), snowgrid := "Agnes"]
snows[grep("jo", origin), snowgrid := "Jo"]
snows[grep("kloo", origin), snowgrid := "Kloo"]

#fix up other data cols
snows[, COMMENTS := NULL]
snows[, Date := lubridate::dmy(DATE)]
setnames(snows, "OPEN SD", "SD")

snow <- snows[, .(Date, snowgrid, SD)]



# foraging data -----------------------------------------------------------

#create year and month column
beh[, m := month(Date)]
beh[, y := year(Date)]

#name the winter months
wintermonths <- c(1, 2, 3, 11, 12)

#cut to only winter 
beh <- beh[m %in% wintermonths]

#create a winter column
beh[m < 4, winter := paste0(y-1, "-", y)]
beh[m > 8, winter := paste0(y, "-", y+1)]

#swap B's in the collar data for 2's
beh[, id := gsub("B", "2", id)]

#pull out the grid of every individual using the trapping data
trapping[, Eartag := as.character(Eartag)]
grids <- trapping[, getmode(grid), Eartag]
names(grids) <- c("id", "grid")

#merge grids into behaviour data set
beh <- merge(beh, grids, by = "id", all.x = TRUE)

#now we need to match grid in behaviour data set to the snow data set.
#more grids with collar data than grids with snow data

#when grid with bunny is one of the snow grids, just copy to new col snowgrid
beh[grid == "Agnes" | grid == "Kloo" | grid == "Jo", snowgrid := grid]

#all other grids and their closest snow grid, but where is leroy?
beh[grid == "Sulphur" | grid == "Rolo" | grid == "Chadbear" | grid == "Leroy", snowgrid := "Kloo"]
beh[grid == "Chitty", snowgrid := "Agnes"]


# merge snow data with behaviour data -------------------------------------------------------------------

full <- merge(beh, snow, by = c("Date", "snowgrid"), all.x = TRUE)

#remove values without snow depth for now

full <- full[!is.na(SD)]

# figures -----------------------------------------------------------------


ggplot(full)+
  geom_point(aes(x = SD, y = Forage, color = as.factor(m)))

ggplot(full)+
  geom_point(aes(x = SD, y = Hopping))

ggplot(full)+
  geom_point(aes(x = SD, y = notmoving))

ggplot(full)+
  geom_point(aes(x = SD, y = Sprinting))


summary(lm(Forage ~ SD, full))




winter1617 <- 
  ggplot()+
  geom_point(aes(x = Date, y = Forage/3600), data = beh[winter == "2016-2017"])+
  geom_point(aes(x = Date, y = Forage/3600), data = forageday[ winter == "2016-2017"], size = 1.5, color = "red")+
  labs(x = "Date", y = "Forage effort (hr/day)", title = "2016-2017")+
  theme_minimal()

winter1415 <- 
  ggplot()+
  geom_point(aes(x = Date, y = Forage/3600), data = beh[winter == "2014-2015"])+
  geom_point(aes(x = Date, y = Forage/3600), data = forageday[ winter == "2014-2015"], size = 1.5, color = "red")+
  labs(x = "Date", y = "Forage effort (hr/day)", title = "2014-2015")+
  theme_minimal()


ggsave("Output/figures/winter1617.jpg", winter1617, width = 4.5, height = 3.5, unit = "in")

ggsave("Output/figures/winter1415.jpg", winter1415, width = 4.5, height = 3.5, unit = "in")


