# script for translating biomass and nutritional compositions into total nutrients 

library(data.table)
library(ggplot2)

sums <- readRDS("Output/Data/transect_biomass.rds")
nuts <- fread("Input/Plant_nutrition.csv")



# prep biomass data -------------------------------------------------------

#create data frame called heights. This shows one column with three options for height class
sums[, total := NULL]
heights <- melt(sums, measure.vars = c("low", "med", "high"), variable.name = "Height", value.name = "Biomass")
heights[Height == "med", Height := "medium"]



# prep nutrition data for merge -------------------------------------------

#create species column with sample id
nuts[grep("S", Sample), Species := "spruce"]
nuts[grep("W", Sample), Species := "willow"]

#create height column with sample id
nuts[grep("L", Sample), Height := "low"]
nuts[grep("M", Sample), Height := "medium"]
nuts[grep("H", Sample), Height := "high"]

#remove random value with no speices or height
nuts <- nuts[!is.na(Species)]


#create data table of mean nutritional values by species and height
### EVENTUALLY ADD IN grid
meannuts <- nuts[, .(mean(NDF_F, na.rm = TRUE), mean(ADF_F, na.rm = TRUE), 
                     mean(ADL_F, na.rm = TRUE), mean(CP_F, na.rm = TRUE)), by = .(Species, Height)]
names(meannuts) <- c("Species", "Height", "NDF", "ADF", "ADL", "CP")

#create a melted version of nutrient data, melted by nutrient
justnuts <- nuts[, .(NDF_F, ADF_F, ADL_F, CP_F, Species, Height)]
justnuts <- melt(justnuts, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F"), variable.name = "Nutrient", value.name = "Composition")

#make height class a leveled factor
justnuts[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

#rename nutrients for figures
justnuts[, Nutrient := gsub("_F", "", Nutrient)]

#figure to look at difference between height classes if any
ggplot(justnuts)+
  geom_boxplot(aes(x = Species, y = Composition, fill = Height))+
  theme_minimal()+
  facet_wrap(~ Nutrient, scales = "free")



# merge nutrients and starting biomass ------------------------------------

#initial merge
full <- merge(heights, meannuts, by = c("Species", "Height"), all.x = TRUE)

#calculate biomass of protein
full[, CPmass := Biomass*(CP/100)]

#make height a factor leveled
full[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

ggplot(full)+
  geom_boxplot(aes(x = Species, y = CPmass, fill = Height))+
  ylim(0, 15)+
  theme_minimal()
  