

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/willow_avail_bysite.rds")



# model willow availability for each height -------------------------------

lowgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "low"])
summary(lowgam)

medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])
summary(medgam)

highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])
summary(highgam)

mods <- list(lowgam, medgam, highgam)


modout <- rbindlist(lapply(mods, getgam))


# Predict willow availabilitybased on GAM  --------------------------------------------

#make data set of snow in cm
newdatlow <- data.table(Snow = seq(1, 150), height = "low")
newdatmed <- data.table(Snow = seq(1, 150), height = "medium")
newdathigh <- data.table(Snow = seq(1, 150), height = "high")

#predictions using the gam outputs and new data
predlow <- predict(lowgam, newdatlow, type = 'response', se.fit = TRUE) 
predmed <- predict(medgam, newdatmed, type = 'response', se.fit = TRUE) 
predhigh <- predict(highgam, newdathigh, type = 'response', se.fit = TRUE) 

#fill in new data with predicted value and lower/upper confidence limited
newdatlow[, pred := predlow$fit]
newdatlow[, lower := predlow$fit - 1.96 * predlow$se.fit]
newdatlow[, upper := predlow$fit + 1.96 * predlow$se.fit]

newdatmed[, pred := predmed$fit]
newdatmed[, lower := predmed$fit - 1.96 * predmed$se.fit]
newdatmed[, upper := predmed$fit + 1.96 * predmed$se.fit]

newdathigh[, pred := predhigh$fit]
newdathigh[, lower := predhigh$fit - 1.96 * predhigh$se.fit]
newdathigh[, upper := predhigh$fit + 1.96 * predhigh$se.fit]

#rbind all heights of data
newdat <- rbind(newdatlow, newdatmed, newdathigh)
newdat[, height := factor(height, levels = c("low", "medium", "high"))]



(willow_pred <-
  ggplot(newdat)+
  geom_ribbon(aes(x = Snow, ymin = lower, ymax = upper), alpha = 0.5, fill = "grey70")+
  geom_path(aes(x = Snow, y = pred), color = "blue4", linewidth = .75)+
  labs(x = "Snow depth (cm)", y = "Predicted twig availablity")+
  facet_wrap(~height)+
  theme_minimal(base_size = 16))


#plot using geom smooth GAM
(willow_gam <- 
  ggplot(willow)+
  geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.5, color = "grey50")+
  geom_smooth(aes(x = Snow, y = propavail_willow), method = "gam")+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  facet_wrap(~ height)+
  theme_minimal())



# save predictions --------------------------------------------------------

saveRDS(newdat, "Output/Data/willow_avail_prediction.rds")
ggsave("Output/Figures/Willow_avail_gam.jpeg", willow_gam, width = 9, height = 5, unit = "in")
ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 9, height = 4, unit = "in")
