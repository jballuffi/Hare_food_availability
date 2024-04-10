#script for converting twig compositions to quality related measure

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned twig composition
nuts <- readRDS("Output/Data/cleaned_compositions.rds")
#results from feeding trials
days <- readRDS("../NutritionalGeometryHares/Output/data/dailyresultscleaned.rds")



# summarize nutrition compositions ----------------------------------------

#summary figure of plant compositions
ggplot(nuts)+
  geom_boxplot(aes(y = Composition, x = Species, color = Height))+
  facet_wrap(~ Nutrient, scales = "free")

#calculate diet carb content and carb intake rate, but redo this in the geometry project!!!!!
days[, Carb_diet := 1 - (CP_diet + NDF_diet)]
days[, DMI_Carb := DMI*Carb_diet]

#summary table of plant composition ranges and diets
nuts[, .(min = min(Composition), max = max(Composition)), by = Nutrient]
days[, .(cp = mean(CP_diet), ndf = mean(NDF_diet), adf = mean(ADF_diet), adl = mean(ADL_diet), carb = mean(Carb_diet)), Diet]



# predict digestibility from feeding trial data ------------------------------


modcp <- lm(DMD ~ poly(CP_diet, 3), data = days)
CP_dat <- data.frame(CP_diet = seq(min(days$CP_diet), max(days$CP_diet), length = 100))
predictions <- predict(modcp, newdata = CP_dat)

plot(days$CP_diet, days$DMD)
lines(CP_dat$CP_diet, predictions, col = "red")


modcarb <- lm(DMD ~ poly(Carb_diet, 3), data = days)
CP_dat <- data.frame(CP_diet = seq(min(days$CP_diet), max(days$CP_diet), length = 100))
predictions <- predict(mod, newdata = CP_dat)

plot(days$CP_diet, days$DMD)
lines(CP_dat$CP_diet, predictions, col = "red")




DMDpred <- data.table(
  DMI_Carb = min(days$DMI_Carb):max(days$DMI_Carb),
  DMI_CP = min(days$DMI_CP):max(days$DMI_CP)
)




# additional polynomial regression for predicting DMD with all diets----------------

#polynomial regression between protein:NDF and DMD
digmod <- lm(DMD ~ poly(CP_NDF, 2), days)
summary(digmod)

#plot relationship between CP:NDF and DMD, for all diets
ggplot(days, aes(x = CP_NDF, y = DMD))+
  geom_point()+
  geom_smooth(method="lm", formula = y ~ poly(x, 2))+
  labs(x = "CP:NDF", y = "DMD (%)")+
  theme_minimal()





# create a summary datatable for export -----------------------------------

# this will be merged with biomass and availability later

#create data table of means, medians, and standard deviations for each nutritional value by species and height
meannuts <- nuts[, .(mean(DMD, na.rm = TRUE), mean(CP_F, na.rm = TRUE), 
                     median(DMD, na.rm = TRUE), median(CP_F, na.rm = TRUE),
                     sd(DMD, na.rm = TRUE), sd(CP_F, na.rm = TRUE)), 
                 by = .(Species, Height)]

names(meannuts) <- c("species", "height", "DMD_mean", "CP_mean",
                     "DMD_median", "CP_median",
                     "DMD_sd",  "CP_sd")





saveRDS(meannuts, "Output/Data/starting_nutrition.rds")
ggsave("Output/Figures/DMD_by_height.jpeg", justDMD, width = 6, height = 5, unit = "in")



