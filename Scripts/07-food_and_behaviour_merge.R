

library(data.table)
library(lubridate)
library(ggplot2)

#get mode function
#this function is from the R folder in the footload project
getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# read in files -----------------------------------------------------------

#read in food availability final datasheet
food <- readRDS("Output/Data/Total_daily_food_availability.rds")

#list just the daily foraging files and fread in
daily <- list.files("Output/Data/Axy_behaviours/", pattern = "daily", full.names = TRUE)
axy <- lapply(daily, fread)

#for now remove problematic file
axy[[11]] <- NULL

#bind all files into one
axy <- rbindlist(axy)

#read in trapping data
trapping <- fread("Input/Trapping_data_all_records.csv")



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
food[grid == "JO", grid := "Jo"][grid == "SU", grid := "Sulphur"][grid == "KL", grid := "Kloo"]

#final merge, by date and grid
#every day of animal behaviour should be paired to a snow depth, temp, biomass availability, and DMD availability 
fullaxy <- merge(beh, food, by = c("date", "grid"), all.x = TRUE)



# merge food data with weight data ----------------------------------------


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
  food[date %in% datelist & grid %in% haregrid, .(mean(snowdepth), mean(temp), mean(biomassavail), mean(DMDavail))]
}

weightavgfood <- weights[, weeklyfood(weekdate = .BY[[1]], haregrid = grid), by = .(date, ID)]
names(weightavgfood) <- c("date", "ID", "snowdepth", "temp", "biomassavail", "DMDavail")
weightavgfood <- weightavgfood[duplicated(weightavgfood) == FALSE]

fullweight <- merge(weights, weightavgfood, by = c("date","ID"))




# save --------------------------------------------------------------------

saveRDS(fullaxy, "Output/Data/Full_data_behandfood.rds")
saveRDS(fullweight, "Output/Data/weight_data.rds")
write.csv(axy, "Output/Data/axy_behaviours_2023.csv")
