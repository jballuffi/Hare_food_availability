#script for converting twig compositions to quality related measure

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned twig composition
nuts <- readRDS("Output/Data/cleaned_compositions.rds")

#results from feeding trials
days <- readRDS("../NutritionalGeometryHares/Output/data/dailyresultscleaned.rds")
trials <- readRDS("../NutritionalGeometryHares/Output/data/trialresultscleaned.rds")
diets <- fread("../NutritionalGeometryHares/Input/Diet_compositions.csv")



# predict digestibility from feeding trial data ------------------------------

DMD <- gam(DMD ~ s(DMI_CP, DMI_NDF, DMI_ADF), data = days)

DMDpred <- data.table(
  DMI_NDF = min(days$DMI_NDF):max(days$DMI_NDF),
  DMI_CP = min(days$DMI_CP):max(days$DMI_CP),
  DMI_ADF = min(days$DMI_ADF):max(days$DMI_ADF)
)

DMDpred$DMD <- predict.gam(DMD, DMDpred)

ggplot(DMDpred)+
  geom_point(aes(x = DMI_CP, y = DMD, color = DMI_NDF))

ggplot(days)+
  geom_point(aes(x = DMI_NDF, y = DMD))





diets[, Carb_diet := 100 - (CP_diet + NDF_diet)]
diets[, CP_Carb := CP_diet/Carb_diet]

ggplot(diets)+
  geom_boxplot(aes(x = Sample, y = CP_Carb))

ggplot(days)+
  geom_point(aes(x = DP, y = CP))+
  geom_smooth(aes(x = DMDI, y = Weight_change))




days[, Carb_diet := 1 - (mean(CP_diet, na.rm = TRUE) + mean(NDF_diet, na.rm = TRUE)), Diet]
days[, CP_Carb := CP_diet/Carb_diet]

ggplot(days)+
  geom_point(aes(x = CP_diet/Carb_diet, y = DMD))

diets[, "CP_diet"]

class(diets)


#create CP:ADL ratio
days[, CP_fibre := CP_diet/NDF_diet/ADL_diet]

plot(days$DP ~ days$CP_fibre)

test <- lm(DP ~ CP_fibre, days)
summary(test)


days[, mean(DNDF)*100]
days[, mean(DADL)*100, Diet]
days[, mean(DADL), Diet]


#NDF * what can be digested... plus digestible protein?



ggplot(days)+
  geom_point(aes(x = CP_ADL, y = DMD))



#linear regression between protein:ndf ratio for just diet A and B 
digmodAB <- lm(DMD ~ NDF_diet, daysAB)
summary(digmodAB)
dint <- coef(digmodAB)["(Intercept)"]   # get intercept
dsl <- coef(digmodAB)["NDF_diet"]         # get slope

#relationship between CP:NDF and DMD for just A and B
ggplot(days, aes(x = CP_NDF, y = DMD))+
  geom_point()+
  #geom_abline(intercept = dint, slope = dsl)+
  labs(x = "Proportion NDF", y = "Proportion DMD")+
  theme_minimal()



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




# Using feeding trial digestibility regression, estimate DMD --------------

#calculate CP:NDF ratios
nuts[, CP_NDF := CP_F/NDF_F]

#use CP:NDF ratio to predict DMD
nuts[, DMD := dint + (dsl*NDF_F/100)]


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



