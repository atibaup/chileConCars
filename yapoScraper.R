# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 

yapoBrandModelCodes <- list(list(make = 'Toyota', model = '4Runner', makeCode = 88, modelCode = 1),
                         list(make = 'Toyota', model = 'LandCruiser', makeCode = 88, modelCode = 15),
                         list(make = 'Nissan', model = 'Pathfinder', makeCode = 64, modelCode = 30),
                         list(make = 'Nissan', model = 'X-Trail', makeCode = 64, modelCode = 45),
                         list(make = 'Mitsubishi', model = 'Montero', makeCode = 62, modelCode = 11),
                         list(make = 'Suzuki', model = 'GrandVitara', makeCode = 86, modelCode = 15))

carData <- NULL
for (modelInfo in yapoBrandModelCodes) {
  print(sprintf("Fetching data for %s %s", modelInfo$make, modelInfo$model))
  newData <- getYapoCarsDataFrameForModel(modelInfo)
  if (is.null(carData)) {
    carData <- newData
  } else {
    carData <- rbind(carData, newData)
  }
}

write.csv(carData, file = "yapoData.csv")
