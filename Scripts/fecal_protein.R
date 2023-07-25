
library(data.table)
library(ggplot2)
library(ggeffects)
library(lubridate)

fec <- fread("Input/fecal_CP.csv")

#fill in missing DMs
meandm <- fec[, mean(DM, na.rm = TRUE)]
fec[is.na(DM), DM := meandm]
fec[is.na(CP_F), CP_F := CP_AI/DM]

#take grid and date from sample column
fec[, date := tstrsplit(Sample, " ", keep = 2)]
fec[, loc := tstrsplit(Sample, " ", keep = 1)]
fec[, date := dmy(date)]

fec[loc == "BT" | loc == "JO", grid := "Jo"]
fec[loc == "KL" | loc == "KW", grid := "Kl"]
fec[loc == 'LL' | loc == "RL" | loc == "SU", grid := "Su" ]
fec[loc == "AG", grid := "AG"]


ggplot(fec[!grid == "AG"])+
  geom_point(aes(x = date, y = CP_F, color = grid))+
  geom_smooth(aes(x = date, y = CP_F, group = grid, color = grid))
