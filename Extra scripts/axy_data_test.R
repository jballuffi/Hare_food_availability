

library(data.table)
library(lubridate)
library(ggplot2)



# converts <-  list.files("Output/Data/Axy_behaviours/", pattern = "convert", full.names = TRUE)
# convert <- lapply(converts[1:5], fread)
# sample <- rbindlist(convert, use.names = TRUE, idcol = 'origin')
# sample[, origin := factor(origin, labels = basename(converts[1:5]))]



sample <- fread("Output/Data/Axy_behaviours/convert_26664_23-4-2023.csv")


sample[, Date := dmy(date)]
sample[, mindate := min(Date)]
sample[, diffdate := Date - mindate]
sample[, dt := paste0(date, " ", time)]
sample[, dt_class := dmy_hms(dt)]

sampleday <- sample[diffdate < 5]



#test <- sample[, mean(odba), by = .(Date, origin)]


out <- 
  ggplot(sampleday)+
  geom_point(aes(y = odba, x = dt_class), size = 0.5, alpha = 0.5, shape = 1)+
  ylim(0, 17.5)

ggsave("Output/Figures/26664.jpeg", width = 7, height = 6, units = "in")
