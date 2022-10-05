library(ggplot2)
library(data.table)

sums <- readRDS("Output/Data/transect_biomass.rds")


# asymptote work ----------------------------------------------------------


RandomNum <- function(resp){
  #set effort, increase by one transect until total sample reached
  effort <- c(1: length(unique(resp)))
  sample(effort, replace = FALSE)
}

sums[, Number := RandomNum(Loc), by = .(Grid, Species)]

effort2 <- c(1: length(unique(sums$Number)))

draw <- lapply(effort2, function(n) {
  sample <- sums[Number < n+1, .(mean(Biomass), sd(Biomass)), by = .(Grid, Species)]
  names(sample) <- c("Grid", "Species", "Mean", "SD")
  sample[, Effort := n]
  return(sample)
}) 

asymstats <- rbindlist(draw)


draw <- lapply(effort2, function(n) {
  sample <- sums[Number < n+1, mean(Biomass)]
  return(sample)
}) 






fullfun <- function(DT, x, y){
  
  effort <- c(1: length(unique(x)))
  
  DT[, Number := sample(effort, replace = FALSE)]
  
  draw <- lapply(effort, function(n) {
    DT[Number < n+1, mean(y)]
  }) 
 
  totals <- rbindlist(draw)
  
  return(totals)
}


test <- sums[, fullfun(DT = .SD, y = Biomass, x = Loc), by = c("Grid", "Species")]








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


