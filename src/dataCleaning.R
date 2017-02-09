# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 
source("scrapingLib.R")

yapoData <- read.csv("../data/yapoDataNew.csv")
yapoData$X <- NULL
yapoData$source <- "Yapo"

chileautosData <- read.csv("../data/chileautosData.csv")
chileautosData$X <- NULL
chileautosData$source <- "Chileautos"

carData <- rbind(yapoData, chileautosData)

# data cleaning:
# 1) Remove entries with silly kilómetros
minKm = 10
maxKm = 5e5
cleanCarData = subset(carData, carData$Kilómetros > minKm & carData$Kilómetros < maxKm)

# 2) Remove entries with obviously spurious prices
minPrice <- 4000
maxPrice <- 50000
cleanCarData = subset(cleanCarData, 
                      cleanCarData$Precio.USD >= minPrice & cleanCarData$Precio.USD <= maxPrice)

# 3) Remove entries with bad year data
cleanCarData = subset(cleanCarData, !(model == "4Runner" & Edad == 26 & Precio.USD > 10000))

# 4) Remove entries with Combustible = "Gas" or "Otros" since they add up to 5 samples only
cleanCarData = subset(cleanCarData, !(Combustible %in% c("Gas", "Otros")))

# 5) Fix cars from the future
cleanCarData[cleanCarData$Edad<0,]$Edad = 0

cleanCarData$Kilómetros.miles = cleanCarData$Kilómetros / 1000

write.csv(cleanCarData, file = "../data/cleanCarData.csv")