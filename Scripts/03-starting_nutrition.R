
# script for exploring nutritional compositions of twigs by species and height 

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#these are the results for plant samples taken before winter 2022-2023
nuts <- fread("Input/Plant_nutrition.csv")



# prep variable names  -------------------------------------------

#create species column with sample id
nuts[grep("S", Sample), Species := "spruce"]
nuts[grep("W", Sample), Species := "willow"]

#create height column with sample id
nuts[grep("L", Sample), Height := "low"]
nuts[grep("M", Sample), Height := "medium"]
nuts[grep("H", Sample), Height := "high"]

#remove random value with no species or height
nuts <- nuts[!is.na(Species)]

#subset to main variables
justnuts <- nuts[, .(Species, Height, Grid, Loc, CP_F, NDF_F, ADF_F, ADL_F)]



# make data long ----------------------------

#create a melted version of nutrient data, melted by nutrient
justnuts <- melt.data.table(justnuts, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F"), variable.name = "Nutrient", value.name = "Composition")

#make height class a leveled factor
justnuts[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

#rename nutrients for figures
justnuts[, Nutrient := gsub("_F", "", Nutrient)]

#remove NAs
justnuts <- justnuts[!is.na(Composition)]



# look at basic composition trends by height ------------------------------

#figure to look at difference between height classes if any, switched plot to just willow for preliminary stuff
(allnuts <- 
    ggplot(justnuts)+
    geom_boxplot(aes(x = Species, y = Composition, fill = Height), alpha = 0.4)+
    labs(y = "Composition (%)", x = "Browse height")+
    theme_minimal()+
    facet_wrap(~ Nutrient, scales = "free"))

#explore average compositions by species and height 
avgnut <- justnuts[, .(Composition = mean(Composition)), by = .(Species, Height, Nutrient)]

#look at significant differences between nutritional compositions of different heights
sig <- justnuts[, lm_out(lm(Composition ~ Height)), by = .(Species, Nutrient)]



# save outputs ------------------------------------------------------------

saveRDS(justnuts, "Output/Data/cleaned_compositions.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 8, height = 6, unit = "in")

