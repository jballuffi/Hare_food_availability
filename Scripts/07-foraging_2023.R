

library(data.table)
library(lubridate)
library(ggplot2)


# read in files -----------------------------------------------------------

#list just the daily foraging files and fread in
daily <- list.files("Output/Data/Axy_behaviours/", pattern = "daily", full.names = TRUE)
axy <- lapply(daily, fread)

#for now remove problematic file
axy[[11]] <- NULL

#bind all files into one
axy <- rbindlist(axy)

#read in trapping data
trapping <- fread("Input/Trapping_data_all_records.csv")

#get mode function
#this function is from the R folder in the footload project
getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# get individual info from trapping data ----------------------------------

#make eartag an ID as character
trapping[, ID := as.character(Eartag)]

#make date a date
trapping[, Date := dmy(dateCap)]

#pull out the grid and sex of every individual
info <- trapping[, .(getmode(grid), getmode(Sex)), ID]
names(info) <- c("ID", "grid", "sex")

#make sex factor
info[, sex := as.factor(sex)]

#if sex = 1 it's a male, if sex = 2 it's a femal
info[sex == 1, sex := "male"]
info[sex == 2, sex := "female"]

trapnights <- trapping[, Date, ID]
trapnights[, trapped := "yes"]



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
beh <- beh[!trapped == "yes"]


#next merge in snow data and final nutrient availability data
#what do I want the final datasheet to look like? 


# Look into data ----------------------------------------------------------



ggplot(beh)+
  geom_point(aes(x = Date, y = Forage, color = sex))


ggplot(beh)+
  geom_point(aes(x = Date, y = Sprinting, color = sex))
