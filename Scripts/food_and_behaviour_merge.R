
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# read in files -----------------------------------------------------------

#read in food availability final datasheet
food <- readRDS("Output/Data/Total_daily_food_availability.rds")

#read in trapping data
trapping <- fread("Input/Trapping_data_all_records.csv")


#list just the daily foraging files and fread in
daily <- list.files("../Process_axy_data/Output/Data/Axy_behaviours/", pattern = "daily", full.names = TRUE)
axy <- lapply(daily, fread)

#bind all files into one
axy <- rbindlist(axy, fill = TRUE)
#take only first 7 columns
axy <- axy[, 1:7]




# get individual info from trapping data ----------------------------------

#list individuals in axy data
inds <- axy[, unique(ID)]

#make eartag an ID as character
trapping[, ID := as.character(Eartag)]

#make date a date
trapping[, Date := dmy(dateCap)]

#cut trapping data to only include individuals collared with axys
trapping <- trapping[ID %in% inds & year(Date) > 2022]

#turn sex 0s to NAs
trapping[Sex == 0, Sex := NA]

#change sex to factor
trapping[, Sex := as.factor(Sex)]

#pull out the grid and sex of every individual
info <- trapping[, .(getmode(grid), getmode(Sex)), ID]
names(info) <- c("ID", "grid", "sex")

#if sex = 1 it's a male, if sex = 2 it's a female
info[sex == 1, sex := "male"]
info[sex == 2, sex := "female"]

#get the nights bunnies were trapped, these will be removed from axy data
trapnights <- trapping[, Date, ID]
trapnights[, trapped := "yes"]

#get just weights
weights <- trapping[Weight > 0, .(Date, ID, grid, Sex, Weight)]
names(weights) <- c("date", "ID", "grid", "Sex", "weight")


# merge individual info with axy data -------------------------------------

#make ID factor in axy
axy[, ID := as.character(ID)]

#get date in lubridate format
axy[, Date := ymd(Date) ]

#remove NAs
axy <- axy[!is.na(ID)]

#merge grids into behaviour data set
beh <- merge(axy, info, by = "ID", all.x = TRUE)

#merge in trap nights
beh <- merge(beh, trapnights, by = c("ID", "Date"), all.x = TRUE)

#remove trap nights from axy data
beh <- beh[is.na(trapped)][, trapped := NULL]

#clean up behaviour names/cols
beh[, V1 := NULL]
setnames(beh, c("Date", "Forage", "Hopping", "Sprinting"), c("date", "foraging", "hopping", "sprinting"))



# merge food data with axy data -------------------------------------------

#make grids match
beh[grid == "Jo", grid := "JO"][grid == "Sulphur", grid := "SU"][grid == "Kloo", grid := "KL"]

setnames(food, "idate", "date")

willow <- food[species == "willow", .(snow = mean(snow, na.rm = TRUE), temp = mean(temp, na.rm = TRUE), 
                 biomass = mean(biomass, na.rm = TRUE), CP_comp = mean(CP_comp, na.rm = TRUE)), 
             by = .(grid, date)]

spruce <- food[species == "spruce", .(snow = mean(snow), temp = mean(temp), 
                                             biomass = mean(biomass), CP_comp = mean(CP_comp)), 
                      by = .(grid, date)]



#final merge, by date and grid
#every day of animal behaviour should be paired to a snow depth, temp, biomass availability, and DMD availability 
willowaxy <- merge(beh, willow, by = c("date", "grid"), all.x = TRUE)

spruceaxy <- merge(beh, spruce, by = c("date", "grid"), all.x = TRUE)



# figures and trends ------------------------------------------------------


ggplot(willowaxy)+
  geom_point(aes(x = date, y = foraging/3600, color = snow))+
  labs(x = "Date", y = "Daily foraging effort (hr)")+
  theme_minimal()

ggplot(willowaxy)+
  geom_point(aes(x = snow, y = foraging/3600))+
  geom_smooth(aes(x = snow, y = foraging/3600), method = "lm")+
  labs(x = "Snow depth (cm)", y = "Daily foraging effort (hr)")+
  theme_minimal()

ggplot(willowaxy)+
  geom_point(aes(x = biomass, y = foraging/3600, color = snow))+
  geom_smooth(aes(x = biomass, y = foraging/3600), method = "lm")+
  labs(x = "Available willow biomass (g/m2)", y = "Daily foraging effort (hr)")+
  theme_minimal()

ggplot(spruceaxy)+
  geom_point(aes(x = biomass, y = foraging/3600, color = snow))+
  geom_smooth(aes(x = biomass, y = foraging/3600), method = "lm")+
  labs(x = "Available spruce biomass (g/m2)", y = "Daily foraging effort (hr)")+
  theme_minimal()


summary(lm(foraging/3600 ~ snow, willowaxy))
summary(lm(foraging/3600 ~ biomass, willowaxy))




# merge food data with weight data ----------------------------------------

# THIS IS BROKEN

#function to pull mean snow depth, temp, biomass, and DMD from the week around each weight data 
weeklyfood <- function(weekdate, haregrid) {
  
  #take three days before home range date and three days after
  datelist <- c(
    weekdate - 3,
    weekdate - 2,
    weekdate - 1,
    weekdate,
    weekdate + 1,
    weekdate + 2,
    weekdate + 3
  )
  
  #in the date list and snow grid of the "snow" data, average the snow depth
  food[date %in% datelist & grid %in% haregrid, .(mean(snow), mean(temp), mean(biomass), mean(CP_comp))]
}

weightavgfood <- weights[, weeklyfood(weekdate = .BY[[1]], haregrid = grid), by = .(date, ID)]

names(weightavgfood) <- c("date", "ID", "snowdepth", "temp", "biomassavail", "DMDavail")
weightavgfood <- weightavgfood[duplicated(weightavgfood) == FALSE]

fullweight <- merge(weights, weightavgfood, by = c("date","ID"))




# save --------------------------------------------------------------------

saveRDS(fullaxy, "Output/Data/Full_data_behandfood.rds")
saveRDS(fullweight, "Output/Data/weight_data.rds")
write.csv(axy, "Output/Data/axy_behaviours_2023.csv")
