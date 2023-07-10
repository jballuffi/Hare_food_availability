
# script for translating biomass and nutritional compositions into total nutrients 

library(data.table)
library(ggplot2)
library(ggeffects)

#these are the results for plant samples taken before winter 2022-2023
nuts <- fread("Input/Plant_nutrition.csv")

#results from feeding trials
days <- readRDS("../NutritionalGeometryHares/Output/data/dailyresultscleaned.rds")
trials <- readRDS("../NutritionalGeometryHares/Output/data/trialresultscleaned.rds")


# show trend for weight change an DMD -------------------------------------

#linear model for diet DMD predicting weight change
weightmod <- lm(Weight_change ~ DMD, trials)
summary(weightmod)
wint <- coef(weightmod)["(Intercept)"]
wsl <- coef(weightmod)["DMD"]

#show relationship between DMD and weight change 
ggplot(trials, aes(x = DMD, y = Weight_change))+
  geom_abline(intercept = 0, slope = 0, linetype = 2, color = "grey65", size = 1)+
  geom_point()+
  geom_abline(intercept = wint, slope = wsl, color = "grey20", size = 1)+
  theme_minimal()



# Predict digestibility from food composition in feeding trials  ----------

#remove problematic response for now
days <- days[! Sample == "E15_23-02-05"]

#create CP:NDF ratio
days[, CP_NDF := CP_diet/NDF_diet]

#cut to just diet A and B
daysAB <- days[Diet == "A" | Diet == "B"]

#linear regression between protein:ndf ratio for just diet A and B 
digmodAB <- lm(DMD ~ NDF_diet, daysAB)
summary(digmodAB)
dint <- coef(digmodAB)["(Intercept)"]   # get intercept
dsl <- coef(digmodAB)["NDF_diet"]         # get slope

#relationship between CP:NDF and DMD for just A and B
ggplot(daysAB, aes(x = NDF_diet, y = DMD))+
  geom_point()+
  geom_abline(intercept = dint, slope = dsl)+
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



# prep variable names for future merge -------------------------------------------

#create species column with sample id
nuts[grep("S", Sample), Species := "spruce"]
nuts[grep("W", Sample), Species := "willow"]

#create height column with sample id
nuts[grep("L", Sample), Height := "low"]
nuts[grep("M", Sample), Height := "medium"]
nuts[grep("H", Sample), Height := "high"]

#remove random value with no speices or height
nuts <- nuts[!is.na(Species)]


# Using feeding trial digestibility regression, estimate DMD --------------

#calculate CP:NDF ratios
nuts[, CP_NDF := CP_F/NDF_F]

#use CP:NDF ratio to predict DMD
nuts[, DMD := dint + (dsl*NDF_F/100)]



# Investigate nutrition trends by height class ----------------------------

#create a melted version of nutrient data, melted by nutrient
justnuts <- nuts[, .(NDF_F, ADF_F, ADL_F, CP_F, DMD, Species, Height, Grid)]
justnuts <- melt(justnuts, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F", "DMD"), variable.name = "Nutrient", value.name = "Composition")

#make height class a leveled factor
justnuts[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

#rename nutrients for figures
justnuts[, Nutrient := gsub("_F", "", Nutrient)]

#figure to look at difference between height classes if any, switched plot to just willow for preliminary stuff
(allnuts <- 
    ggplot(justnuts[!Nutrient == "DMD" & Species == "willow"])+
    geom_boxplot(aes(x = Height, y = Composition))+
    labs(y = "Composition (%)", x = "Browse height")+
    theme_minimal()+
    facet_wrap(~ Nutrient, scales = "free"))

(justDMD <- 
  ggplot(justnuts[Nutrient == "DMD" & Species == "willow"])+
  geom_boxplot(aes(x = Height, y = Composition))+
  labs(y = "Proportion DMD", x = "Browse height")+
  theme_minimal())

summary(lm(Composition ~ Height, data = justnuts[Nutrient == "DMD" & Species == "willow"]))
  

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



# save outputs ------------------------------------------------------------

saveRDS(meannuts, "Output/Data/starting_nutrition.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 8, height = 6, unit = "in")
ggsave("Output/Figures/DMD_by_height.jpeg", justDMD, width = 6, height = 5, unit = "in")


