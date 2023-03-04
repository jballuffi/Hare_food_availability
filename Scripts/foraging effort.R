#script that investigates foraging data from previous winters
#also includes snow depths from those years

library(data.table)
library(ggplot2)
library(lubridate)


beh <- fread("Input/allHareDailyValues2015_2021.csv")
snow <- fread("Input/snowdepthdataentry.csv")




# snow data ---------------------------------------------------------------

snow[, Date := dmy(depthDate)]
setnames(snow, "snow depth", "Snowdepth")

snow <- snow[!Snowdepth < 0]

snow[, y := year(Date)]
snow[, m := month(Date)]


#create a winter column
snow[m < 4, winter := paste0(y-1, "-", y)]
snow[m > 8, winter := paste0(y, "-", y+1)]

snowmonth <- snow[, mean(Snowdepth), .(m, y)]
names(snowmonth) <- c("m", "y", "Snowdepth")

snowday <- snow[, mean(Snowdepth), .(Date, winter)]
setnames(snowday, "V1", "Snowdepth")





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

#look at sample size by year and month
beh[, .N, by = .(y, m)]

#avg foraging by day
forageday <- beh[, mean(Forage), .(Date, winter)]
setnames(forageday, "V1", "Forage") 



# merge -------------------------------------------------------------------

full <- merge(beh, snowmonth, by = c("y", "m"), all.x = TRUE)




# figures -----------------------------------------------------------------


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


