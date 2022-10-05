#script for summary biomass figures

library(data.table)
library(ggplot2)

sums <- readRDS("Output/Data/transect_biomass.rds")

#basic plot showing different between species biomass per transect
ggplot(sums)+
  geom_boxplot(aes(x = Species, y = Biomass, fill = Grid))+
  labs(y = "Dry biomass (g/m2)")+
  theme_minimal()
