
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

#data
startingbiomass <- readRDS("Output/Data/starting_biomass.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition.rds")

#change col names in starting nutrients
setnames(startingnuts, c("Species", "Height"), c("species", "height"))

#predictive data
willow <- readRDS("Output/Data/willow_avail_prediction.rds")
spruce <- readRDS("Output/Data/spruce_avail_prediction.rds")





# Merge predictive dataset with biomass and quality -------------------------------

#change col name in spruce
setnames(spruce, "propavail_spruce", "pred")

#make species column
spruce[, species := "spruce"]
willow[, species := "willow"]

#cut spruce data to 150 cm of snow max
spruce <- spruce[Snow < 151]

#bind spruce and willow prediction
pred <- rbind(spruce, willow, fill = TRUE)

