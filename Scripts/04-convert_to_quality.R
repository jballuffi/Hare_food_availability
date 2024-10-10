#script for converting twig compositions to quality related measure

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in cleaned twig composition
nuts <- readRDS("Output/Data/cleaned_compositions.rds")



# summarize nutrition compositions ----------------------------------------


#summary table of plant composition ranges and diets
nuts[, .(min = min(Composition), max = max(Composition)), by = Nutrient]



# create a summary datatable for export -----------------------------------

# this will be merged with biomass and availability later

#create data table of means, medians, and standard deviations for each nutritional value by species and height
meannuts <- nuts[Nutrient == "CP", .(CP_mean = mean(Composition, na.rm = TRUE),
                     CP_median = median(Composition, na.rm = TRUE),
                     CP_sd = sd(Composition, na.rm = TRUE)), 
                 by = .(Species, Height)]



saveRDS(meannuts, "Output/Data/starting_nutrition.rds")



