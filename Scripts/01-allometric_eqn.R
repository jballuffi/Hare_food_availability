library(data.table)
library(ggplot2)
library(rsq)


#read in willow data
willow <- fread("Input/Allometric_willow_twig.csv")
willow[, species := "willow"]

#read in spruce data
spruce <- fread("Input/Allometric_spruce_twig.csv")
spruce[, species := "spruce"]


#bind willow and spruce data together, 
#fill = true because different height measurements
allo <- rbind(willow, spruce, fill = TRUE)

#calc water content (proportion)
allo[, water := (Wet_sample - Dry_sample)/Wet_sample]

#remove one case where water content is neg
allo[water < 0, water := NA]

#fill in NA water content
allo[, watermean := mean(water, na.rm = TRUE), by = species]
allo[is.na(water), water := watermean]

#calculate total dry
allo[, Dry_total := (1- water)*Wet_total]


#this function extracts coef, R2s, and creates a table of this output
makemod <- function(yvar, xvar1) {
  # Make the model
  model <- lm(yvar ~ xvar1 + 0)
  # Transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  #extract r-squared from model
  rsqOut <- data.table(rsq(model))
  #label the column name for the rsqOut
  names(rsqOut)<-c("rsq")
  # Return combined columns
  return(data.table(coefOut, rsqOut))
}

#run makemod function by species and rename table output
eqn<- allo[, makemod(yvar = Dry_total, xvar1 = BD), by = species]
names(eqn)<- c("Species", "Slope", "R2")


#test plot for relationship between BD and Dry biomass
ggplot(allo)+
  geom_point(aes(x = BD, y = Dry_total, color = species))+
  xlim(0, 56)+
  geom_abline(aes(intercept = 0, slope = eqn[Species == "willow", return(Slope)]), color = "blue3", alpha = 0.6)+
  geom_abline(aes(intercept = 0, slope = eqn[Species == "spruce", return(Slope)]), color = "red3", alpha = 0.6)+
  labs(y = "Dry biomass (g)", x = "Basal diameter (mm)")+
  theme_minimal()
