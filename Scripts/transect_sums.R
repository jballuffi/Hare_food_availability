library(ggplot2)
library(data.table)


source("Scripts/branch_to_twig.R")
dat <- fread("Input/transects.csv")

#number of transects done
dat[, length(unique(Loc)), by = Grid]



# convert to biomass ------------------------------------------------------

#merge transect data with allometric equations
biomass <- merge(dat, eqn, by = "Species", all.x = TRUE)

#calculate biomass from branch BD for each height class
biomass[, Mass_low := (Slope*BD)*(Low/100)]
biomass[, Mass_med := (Slope*BD)*(Med/100)]
biomass[, Mass_high := (Slope*BD)*(High/100)]

#calculate biomass from branch BD for all height classes combined
biomass[, Mass_total := Slope*BD]

#replace is.na with zeros
biomass[is.na(Mass_low), Mass_low := 0]
biomass[is.na(Mass_med), Mass_med := 0]
biomass[is.na(Mass_high), Mass_high := 0]
biomass[is.na(Mass_total), Mass_total := 0]

#sum biomass per transect then divide by size of transect (15 m2)
sums <- biomass[, sum(Mass_total, na.rm = TRUE)/15, by = .(Species, Grid, Loc)]
setnames(sums, "V1", "Biomass")

#create empty sheet of transects and biomass for cases where there is zero biomass of a species
emptyspruce <- biomass[, unique(Loc), by = Grid]
setnames(emptyspruce, "V1", "Loc")
emptyspruce[, Species := "spruce"]

emptywillow <- biomass[, unique(Loc), by = Grid]
setnames(emptywillow, "V1", "Loc")
emptywillow[, Species := "willow"]

empty <- rbind(emptyspruce, emptywillow)

#merge empty sheet with sums sheet and NAs now appear occassionally in the biomass col
sums <- merge(empty, sums, by = c("Grid", "Loc", "Species"), all.x = TRUE)
#convert NAs to zeros
sums[is.na(Biomass), Biomass := 0]

#basic plot showing different between species biomass per transect
ggplot(sums)+
  geom_boxplot(aes(x = Species, y = Biomass, fill = Grid))+
  labs(y = "Dry biomass (g/m2)")+
  theme_minimal()



# asymptote work ----------------------------------------------------------


RandomNum <- function(resp){
  #set effort, increase by one transect until total sample reached
  effort <- c(1: length(unique(resp)))
  sample(effort, replace = FALSE)
}

sums[, Number := RandomNum(Loc), by = .(Grid, Species)]

effort2 <- c(1: length(unique(sums$Number)))

draw <- lapply(effort, function(n) {
  sample <- sums[Number < n+1, .(mean(Biomass), sd(Biomass)), by = .(Grid, Species)]
  names(sample) <- c("Grid", "Species", "Mean", "SD")
  sample[, Effort := n]
  return(sample)
  }) 

asymstats <- rbindlist(draw)






#plot for Mean biomass over sample size
ggplot(asymstats)+
  geom_line(aes(x = Effort, y = Mean, group = Species, color = Species))+
  labs(x = "Sample size (m)", y = "Mean Biomass (g/m2)")+
  facet_wrap(~Grid)+
  theme_minimal()

#plot for SD biomass over sample size
ggplot(asymstats)+
  geom_line(aes(x = Effort, y = SD, group = Species, color = Species))+
  labs(x = "Sample size (m)", y = "Biomass (g/m2) standard deviation")+
  facet_wrap(~Grid)+
  theme_minimal()

ggplot(asymstats)+
  geom_point(aes(x = Effort, y = Mean, color = Grid))+
  geom_errorbar(aes(x = Effort, ymin = Mean - SD, ymax = Mean + SD, color = Grid), alpha = 0.5)+
  facet_wrap(~Species, scales = "free")


# random sample (stan's idea) ---------------------------------------------

effort2 <- c(9, 14, 19, 25)

draw2 <- lapply(effort2, function(n) {
  locs <- sample(sums$Loc, n, replace = FALSE )
  sample <- sums[Loc %in% locs]
  stats <- sample[Species == "spruce", .(round(mean(Biomass), 2), round(sd(Biomass), 2))]
  names(stats) <- c("Mean", "SD")
  stats[, Size := n]
  return(stats)
  
}) 

rbindlist(draw2)

