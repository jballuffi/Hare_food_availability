
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/starting_flag_count.csv")
cams <- readRDS("Output/Data/camtraps.rds")



# get twig availability by height ---------------------------------------------------

#remove extra underscore from loc
flags[, loc := gsub("_", "", loc)]

#make new full loc column
flags[, Location := paste0(grid, "_", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = c("Location", "winter"), all.x = TRUE)

twigs[, orangeC := as.integer(`1_orange`)][, yellowC := as.integer(`2_yellow`)][, pinkC := as.integer(`3_pink`)]

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := orangeC/orange]
twigs[, medium := yellowC/yellow]
twigs[, high := pinkC/pink]
twigs[, allheights := (orangeC + yellowC + pinkC)/(orange + yellow + pink)]

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, winter, Date, Snow, Temp, Moon, grid, loc, low, medium, high, allheights)]

#melt twig availability by height class
willow <- data.table::melt(twigs, measure.vars = c("low", "medium", "high", "allheights"), variable.name = "height", value.name = "propavail_willow")

#convert farenheit to celcius
willow[, Temp := as.integer(Temp)]
willow[, temp := (Temp-32)/1.8]

willow <- willow[order(Date)]

willow[propavail_willow > 1, propavail_willow := 1]



# Trends -----------------------------------------------------------------

heightcols <- c("low" = "red2", "medium" = "orange", "high" = "blue", "allheights" = "grey15")


#proportion available over time for each location
ggplot(willow[height == "allheights"])+
  geom_path(aes(x = Date, y = propavail_willow, group = Location))+
  labs(y = "Proportion of twigs available", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  themepoints

#get mean proportion available and snow depth by date and height class
means <- willow[, .(prop = mean(propavail_willow, na.rm = TRUE), snow = mean(Snow, na.rm = TRUE)),
                by = .(winter, Date, height)]

#order by date for path plots
means <- means[order(Date)]

#proportion available for each height over time
(proptrend <- 
  ggplot(means)+
  geom_path(aes(x = Date, y = prop, group = height, color = height), linewidth = 1)+
  labs(y = "Proportion of twigs available", x = "Date")+
  scale_color_manual(values = heightcols)+
  facet_wrap(~winter, scales = "free")+
  themepoints)

#snow depth over time
(snowtrend <- 
  ggplot(means)+
  geom_path(aes(x = Date, y = snow), linewidth = 1)+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  themepoints)

#combine proportion available and snow depth into one figure
timetrend <- ggarrange(proptrend, snowtrend, ncol = 1, nrow = 2)

#trend of total proportion available in response to snow depth
(propandsnow <-
  ggplot(willow[height == "allheights"])+
  geom_point(aes(x = Snow, y = propavail_willow))+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  themepoints)

# save output data --------------------------------------------------------

#save the daily measures of avail by camera trap site
saveRDS(willow, "Output/Data/willow_avail_bysite.rds")
ggsave("Output/Figures/Willowavail_snow_time.jpeg", timetrend, width = 8, height = 8, unit = "in" )
ggsave("Output/Figures/Willowavail_over_snow.jpeg", propandsnow, width = 8, height = 6, unit = "in" )
