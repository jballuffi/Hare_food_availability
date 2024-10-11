
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

#subset camera trap data to just proportions and info 
twigs <- twigs[, .(Location, winter, Date, Snow, Temp, Moon, grid, loc, low, medium, high)]

#melt twig availability by height class
willow <- data.table::melt(twigs, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail_willow")

#convert farenheit to celcius
willow[, Temp := as.integer(Temp)]
willow[, temp := (Temp-32)/1.8]

willow <- willow[order(Date)]

willow[propavail_willow > 1, propavail_willow := 1]



# Trends -----------------------------------------------------------------

heightcols <- c("low" = "red2", "medium" = "orange", "high" = "blue")


#get mean proportion available and snow depth by date and height class
means <- willow[, .(propmean = mean(propavail_willow, na.rm = TRUE),
                    propsd = sd(propavail_willow, na.rm = TRUE),
                    snowmean = mean(Snow, na.rm = TRUE),
                    snowsd = sd(Snow, na.rm = TRUE)),
                by = .(winter, Date, height)]

#order by date for path plots
means <- means[order(Date)]

#proportion available for each height over time
(proptrend <- 
  ggplot(means)+
    geom_ribbon(aes(x = Date, ymax = propmean + propsd, ymin = propmean - propsd, fill = height), alpha = 0.2)+
    geom_path(aes(x = Date, y = propmean, group = height, color = height), linewidth = 1)+
    labs(y = "Proportion of twigs available", x = "Date")+
    scale_color_manual(values = heightcols)+
    scale_fill_manual(values = heightcols)+
    facet_wrap(~winter, scales = "free")+
    theme_minimal())

#snow depth over time
(snowtrend <- 
  ggplot(means)+
  geom_ribbon(aes(x = Date, ymin = snowmean - snowsd, ymax = snowmean + snowsd), alpha = 0.2)+
  geom_path(aes(x = Date, y = snowmean), linewidth = 1)+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  theme_minimal())


#combine proportion available and snow depth into one figure
timetrend <- ggarrange(proptrend, snowtrend, ncol = 1, nrow = 2)

#trend of total proportion available in response to snow depth
(propandsnow <-
  ggplot(willow)+
  geom_point(aes(x = Snow, y = propavail_willow, color = height))+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  theme_minimal())

ggplot(willow[height == "high"])+
  geom_point(aes(x = Snow, y = propavail_willow))+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  theme_minimal()



# model willow availability for each height -------------------------------



summary(lm(propavail_willow ~ Snow, willow[height == "low"]))
lowgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "low"])


summary(lm(propavail_willow ~ Snow*Temp + grid, willow[height == "medium"]))
medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])

summary(lm(propavail_willow ~ Snow, willow[height == "high"]))
highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])





# save output data --------------------------------------------------------

#save the daily measures of avail by camera trap site
saveRDS(willow, "Output/Data/willow_avail_bysite.rds")
ggsave("Output/Figures/Willowavail_snow_time.jpeg", timetrend, width = 8, height = 8, unit = "in" )
ggsave("Output/Figures/Willowavail_over_snow.jpeg", propandsnow, width = 8, height = 6, unit = "in" )
