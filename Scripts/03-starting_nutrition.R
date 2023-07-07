# script for translating biomass and nutritional compositions into total nutrients 

library(data.table)
library(ggplot2)

nuts <- fread("Input/Plant_nutrition.csv")



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



# Investigate nutrition trends by height class ----------------------------

#create a melted version of nutrient data, melted by nutrient
justnuts <- nuts[, .(NDF_F, ADF_F, ADL_F, CP_F, Species, Height, Grid)]
justnuts <- melt(justnuts, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F"), variable.name = "Nutrient", value.name = "Composition")

#make height class a leveled factor
justnuts[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

#rename nutrients for figures
justnuts[, Nutrient := gsub("_F", "", Nutrient)]

#figure to look at difference between height classes if any
(allnuts <- 
    ggplot(justnuts)+
    geom_boxplot(aes(x = Species, y = Composition, fill = Height))+
    labs(y = "Composition (%)", x = "Forage type")+
    theme_minimal()+
    facet_wrap(~ Nutrient, scales = "free"))



# create a summary datatable for export -----------------------------------

# this will be merged with biomass and availability later

#create data table of means, medians, and standard deviations for each nutritional value by species and height
meannuts <- nuts[, .(mean(NDF_F, na.rm = TRUE), mean(ADF_F, na.rm = TRUE), mean(ADL_F, na.rm = TRUE), mean(CP_F, na.rm = TRUE), 
                     median(NDF_F, na.rm = TRUE), median(ADF_F, na.rm = TRUE), median(ADL_F, na.rm = TRUE), median(CP_F, na.rm = TRUE),
                     sd(NDF_F, na.rm = TRUE), sd(ADF_F, na.rm = TRUE), sd(ADL_F, na.rm = TRUE), sd(CP_F, na.rm = TRUE)), 
                 by = .(Species, Height)]

names(meannuts) <- c("species", "height", "NDF_mean", "ADF_mean", "ADL_mean", "CP_mean",
                     "NDF_median", "ADF_median", "ADL_median", "CP_median",
                     "NDF_sd", "ADF_sd", "ADL_sd", "CP_sd")



# save outputs ------------------------------------------------------------

saveRDS(meannuts, "Output/Data/starting_nutrition.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 8, height = 6, unit = "in")
