####################################-
### Axy Conversion Script
### Emily Studd
### Feb 2023
####################################-

####################################-
### load libraries ####
####################################-
library(reshape)
library(data.table)
library(purrr)
library(stringr)
library(zoo)
library(tidyverse)
library(lubridate)


####################################-
### Prep master file ####
####################################-
#cleaning up the master deployment file as this provides the date on and off of the collar to clean axy data, also creates the FileName column so that all info from the deployment can be easily linked to axy data


master <- fread("Axy/Master_collar_sheet.csv")

#Whats the working directory
setwd("Axy")

####################################-
### load functions ####
####################################-

#these functions do the behaviour classification


## Do not change anything in here
diffX<-function(data){
  ab=0
  for (i in 2:12){
    ab[i]<-data[i]- data[i-1]
  }
  r=sum(abs(ab))
  return(r)
}

## Do not change anything in here
diffX4<-function(data){
  ab=0
  for (i in 2:4){
    ab[i]<-data[i]- data[i-1]
  }
  r=sum(abs(ab))
  return(r)
}

## Do not change anything in here
behavclass<-function(axy) {
  axy<-axy %>% mutate(Xs=runmed(X, k=91, "median"), Ys=runmed(Y, k=91, "median"), Zs=runmed(Z, k=91, "median"), Xd=X-Xs, Yd=Y-Ys, Zd=Z-Zs, odba=abs(Xd)+abs(Yd)+abs(Zd)) %>% mutate(Dx=rollapply(data=Xd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dy=rollapply(data=Yd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dz=rollapply(data=Zd, width=12, by=12, FUN=diffX,  align="left", fill=NA), Dodba=Dx+Dy+Dz)
  axy<-axy %>% mutate(Dx=rollapply(data=Xd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dy=rollapply(data=Yd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dz=rollapply(data=Zd, width=4, by=4, FUN=diffX4,  align="left", fill=NA), Dodba4=Dx+Dy+Dz, Todba=rollapply(data=odba, width=4, by=4, FUN=sum,  align="left", fill=NA))
  axy1<-axy %>% mutate(Dodba=zoo::na.locf(Dodba),Dodba4=zoo::na.locf(Dodba4), Todba=zoo::na.locf(Todba))
  
  axy1<-axy1 %>% mutate(All=ifelse(Dodba<=1.15, "notmoving", ifelse(Dodba4<=3, "Forage", ifelse(Todba<=6.5, "Hopping", "Sprinting"))))
  
  return(axy1)
}




## may need to make changes to the next function depending on what the axy file looks like from the latest axy manager.  I provide two alternative functions depending on one change in file format that I know varies between years of axy manager.  You'll have to choose which is best but may have to change the first line more if axy file format has changed more.


#everything we want R to do with each axy file. Loads file, does classification, then calculates a daily summary. 
#saves a conversion file and a daily summary file.
#these two functions below use the functions above
#i should use the first option

# might also want to adjust the last two lines depending where you want the converted file to be saved. 

#use if date and time are in separate columns.  Also double check format of date and adjust the third line of the function as needed.  
convert2019 = function(axyfile){
  axy = fread(axyfile, col.names = c("date", "time", "X", "Y", "Z", "temp", "alt")) #create column names
  axy = behavclass(axy) #run function above
  axy = axy %>% mutate(Date = as.Date(date, "%d-%m-%Y")) 
  byDay = axy %>% group_by(Date, All) %>% summarise(num = n())
  byDay = spread(byDay, All, num )
  master1 = filter(master, FileName == axyfile)
  byDay$id = master1$Bunny.ID
  byDay = filter(byDay, Date > master1$Date.on.Bunny & Date < master1$Date.Off.Bunny)
  ## if you want the converted file saved in a different folder from the original file this needs to be adjusted to include a filepath to the folder you wish to save them in.
  write.csv(byDay, paste(unlist(strsplit(axyfile, "[.]"))[1],"daily.csv", sep = ""))
  write.csv(axy, paste(unlist(strsplit(axyfile, "[.]"))[1], "convert.csv", sep = ""))
}

# use if date and time are combined in one column. Also double check format of date and adjust the fifth line of the function as needed.  
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




####################################-
### Convert axy data to behaviour ####
####################################-
#This is set up for data that is organized in the folder in the following manner: HareDataFolder/BunID/XYZ/axyfiles.  I.e. there is a folder for each bun, that contains two folders, one for GPS and one for XYZ.  The axy files should be labelled as dateOffBun_axyName_BunID_1.csv.  If they aren't organized or labelled in this way, this script will not work.  



#make sure dates are classed as dates
master<-master %>% mutate(Date.on.Bunny=as.Date(Date.on.Bunny, "%d-%b-%y"), Date.Off.Bunny=as.Date(Date.Off.Bunny, "%d-%b-%y"))

# run the conversion on all files in the folder of hare data. # adjust file path as needed.   

hares=dir()               
for(i in 1:length(hares)){
  setwd(paste("Axy",hares[i], sep="/"))
  a=paste(getwd(),"XYZ", sep="/")
  setwd(a)
  files=dir(pattern="*.csv")
  files %>% map(convert2019)
}