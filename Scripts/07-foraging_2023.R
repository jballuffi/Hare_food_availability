





daily <- list.files("Output/Data/Axy_behaviours/", pattern = "daily", full.names = TRUE)

axy <- lapply(daily, fread)

#for now remove problematic file
axy[[11]] <- NULL

axy <- rbindlist(axy)

