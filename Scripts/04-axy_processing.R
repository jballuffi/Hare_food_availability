####################################-
### Axy Conversion Script
### Emily Studd
### Feb 2023
### Edited by Juliana Balluffi-Fry (June 2023)
####################################-

library(reshape)
library(data.table)
library(purrr)
library(stringr)
library(zoo)
library(tidyverse)
library(lubridate)



# read in master collar sheet ---------------------------------------------

master <- fread("Axy_data/Master_collar_sheet.csv")
#make sure dates are classed as dates
master[, Date.on.Bunny := dmy(Date.on.Bunny)]
master[, Date.Off.Bunny := dmy(Date.Off.Bunny)]



# Load functions written by emily.  ---------------------------------------
#these functions do the behaviour classification
# DO NOT CHANGE

diffX<-function(data){
  ab=0
  for (i in 2:12){
    ab[i]<-data[i]- data[i-1]
  }
  r=sum(abs(ab))
  return(r)
}

diffX4<-function(data){
  ab=0
  for (i in 2:4){
    ab[i]<-data[i]- data[i-1]
  }
  r=sum(abs(ab))
  return(r)
}

behavclass<-function(axy) {
  axy<-axy %>% mutate(Xs=runmed(X, k=91, "median"), Ys=runmed(Y, k=91, "median"), Zs=runmed(Z, k=91, "median"), Xd=X-Xs, Yd=Y-Ys, Zd=Z-Zs, odba=abs(Xd)+abs(Yd)+abs(Zd)) %>% mutate(Dx=rollapply(data=Xd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dy=rollapply(data=Yd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dz=rollapply(data=Zd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dodba=Dx+Dy+Dz)
  axy<-axy %>% mutate(Dx=rollapply(data=Xd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dy=rollapply(data=Yd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dz=rollapply(data=Zd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dodba4=Dx+Dy+Dz, Todba=rollapply(data=odba, width=4, by=4, FUN=sum,  align="left", fill=NA))
  axy1<-axy %>% mutate(Dodba=zoo::na.locf(Dodba),Dodba4=zoo::na.locf(Dodba4), Todba=zoo::na.locf(Todba))
  axy1<-axy1 %>% mutate(All=ifelse(Dodba<=1.15, "notmoving", ifelse(Dodba4<=3, "Forage", ifelse(Todba<=6.5, "Hopping", "Sprinting"))))
  return(axy1)
}




# Load function that will run behaviour classifications on lists o --------

# Loads file, does classification, then calculates a daily summary. 
# saves a conversion file and a daily summary file.

# use if date and time are in separate columns. 
convert2019 = function(axyfilepath){
  
  #take just the file name from the file path
  axyfile = tstrsplit(axyfilepath, "/", keep = 4)
  
  #fread in all axy files and give column names
  axy = fread(axyfilepath, col.names = c("date", "time", "X", "Y", "Z", "temp", "alt")) #create column names
  
  #run the behaviour function 
  axy = behavclass(axy) 
  
  #classify date in the axy behaviour output 
  axy[, Date := dmy(date)]
  
  #group behaviours by date
  byDay = axy %>% group_by(Date, All) %>% summarise(num = n())
  byDay = spread(byDay, All, num )
  
  #subset the master sheet to just this file
  master1 = filter(master, FileName == axyfile)
  
  #filter behaviours to only include dates within bunny deployment
  byDay = filter(byDay, Date > master1$Date.on.Bunny & Date < master1$Date.Off.Bunny)
  
  #create an ID column in the behaviour data using whats in master sheet
  byDay$ID <- as.numeric(master1$ID)
  
  #save both original conversion data and the daily behaviours to output folder
  write.csv(byDay, paste0("Output/Data/Axy_behaviours/", "daily_", axyfile))
  write.csv(axy, paste0("Output/Data/Axy_behaviours/", "convert_", axyfile))
}




# Run function on list of file paths --------------------------------------

#create starting directory to Axy folder
hares = dir("Axy_data/")

#within the hare directory get filepaths that are in the XYZ folders and have .csv 
getfiles <- function(hare){
  files <- list.files(paste0("Axy_data/", hare, "/", "XYZ"), pattern = "*csv", full.names = TRUE)
  return(files)
}

#run this file fetching function on all hares listed in the hare directory
files <- lapply(hares, getfiles)
#unlist to get all file paths as characters, then list again
files <- unlist(files)
files <- as.list(files)

#lapply the convert function to this list of file paths
lapply(files, convert2019)






# extra version of the conversion function if date and time are combined in one column.
convert2019a=function(axyfile){
  axy=fread(axyfile, col.names=c("datetime", "X", "Y", "Z", "temp", "alt"))
  axy=mutate(axy, date=substr(datetime,1, 10), time=substr(datetime, 12, 25))
  axy=select(axy, date, time, X, Y, Z, temp, alt)
  axy=behavclass(axy)
  axy=axy %>% mutate(Date=as.Date(date, "%d-%m-%Y"))
  byDay=axy %>% group_by(Date, All) %>% summarise(num=n())
  byDay=spread(byDay, All, num )
  master1=filter(master, FileName==axyfile)
  byDay$id=master1$Bunny.ID
  byDay=filter(byDay, Date>master1$Date.on.Bunny & Date<master1$Date.Off.Bunny)
  ## if you want the converted file saved in a different folder this needs to be adjusted to include a filepath to the folder you wish to save them in.  
  write.csv(byDay, paste(unlist(strsplit(axyfile, "[.]"))[1],"daily.csv", sep="")) 
  write.csv(axy, paste(unlist(strsplit(axyfile, "[.]"))[1], "convert.csv", sep=""))
}


