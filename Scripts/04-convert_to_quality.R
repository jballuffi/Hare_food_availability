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




ggplot(days)+
  geom_point(aes(x = Carb_diet, y = DMD))+
  geom_smooth(aes(x = Carb_diet, y = DMD))

ggplot(days)+
  geom_point(aes(x = CP_diet, y = DMD))+
  geom_smooth(aes(x = CP_diet, y = DMD))


summary(lm(DMD ~ poly(Carb_diet*CP_diet), data = days))


DMD <- gam(DMD ~ s(DMI_Carb, DMI_CP), data = days)

DMDpred <- data.table(
  DMI_Carb = min(days$DMI_Carb):max(days$DMI_Carb),
  DMI_CP = min(days$DMI_CP):max(days$DMI_CP)
)

DMDpred$DMD <- predict.gam(DMD, DMDpred)

ggplot(DMDpred)+
  geom_point(aes(x = DMI_CP, y = DMD))






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



