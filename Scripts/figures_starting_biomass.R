#script for summary biomass figures

library(data.table)
library(ggplot2)

sums <- readRDS("Output/Data/transect_biomass.rds")

#basic plot showing different between species biomass per transect
ggplot(sums)+
  geom_boxplot(aes(x = Species, y = total, fill = Grid))+
  labs(y = "Dry biomass (g/m2)")+
  theme_minimal()



spruce <- sums[Species == "spruce", total, by = .(Grid, Loc)]
setnames(spruce, "total", "sprucetotal")

willow <- sums[Species == "willow", total, by = .(Grid, Loc)]
setnames(willow, "total", "willowtotal")

totals <- merge(spruce, willow, by = c("Grid", "Loc"))


ggplot(totals)+
  geom_point(aes(x = sprucetotal, y = willowtotal))


heights <- sums[,total := NULL]
heights <- melt(heights, measure.vars = c("low", "med", "high"), variable.name = "height", value.name = "Biomass")


ggplot(heights)+
  geom_boxplot(aes(x = height, y = Biomass, fill = Grid))+
  facet_wrap(~Species, scales = "free")+
  theme_minimal()

