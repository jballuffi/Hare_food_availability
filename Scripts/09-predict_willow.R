

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/willow_avail_bysite.rds")




# model willow availability for each height -------------------------------

#make a little 


summary(lm(propavail_willow ~ Snow, willow[height == "low"]))
lowgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "low"])
summary(lowgam)

summary(lm(propavail_willow ~ Snow, willow[height == "medium"]))
medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])
summary(medgam)

summary(lm(propavail_willow ~ Snow*temp, willow[height == "high"]))
highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])
summary(highgam)


low <- predict.gam(lowgam)
med <- predict.gam(medgam)
high <- predict.gam(highgam)


ggplot(willow[height == "low"])+
  geom_point(aes(x = Snow, y = propavail_willow))+
  geom_smooth(aes(x = Snow, y = propavail_willow), method = "gam")+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  theme_minimal()

ggplot(willow[height == "medium"])+
  geom_point(aes(x = Snow, y = propavail_willow))+
  geom_smooth(aes(x = Snow, y = propavail_willow), method = "gam")
labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  theme_minimal()

ggplot(willow[height == "high"])+
  geom_point(aes(x = Snow, y = propavail_willow))+
  geom_smooth(aes(x = Snow, y = propavail_willow), method = "gam")+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  theme_minimal()


