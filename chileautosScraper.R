# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 

chileautosBrandModelCodes <- list(list(make = 'Toyota', model = '4Runner', makeCode = 4, modelCode = '4Runner'),
                         list(make = 'Toyota', model = 'LandCruiser', makeCode = 4, modelCode = 'Land Cruiser'),
                         list(make = 'Nissan', model = 'Pathfinder', makeCode = 17, modelCode = 'Pathfinder'),
                         list(make = 'Nissan', model = 'X-Trail', makeCode = 17, modelCode = 'X-Trail'),
                         list(make = 'Mitsubishi', model = 'Montero', makeCode = 3, modelCode = 'Montero'),
                         list(make = 'Suzuki', model = 'GrandVitara', makeCode = 9, modelCode = 'Grand Vitara'))

carData <- NULL
for (modelInfo in chileautosBrandModelCodes) {
  print(sprintf("Fetching data for %s %s", modelInfo$make, modelInfo$model))
  newData <- getChileautosCarsDataFrameForModel(modelInfo)
  if (is.null(carData)) {
    carData <- newData
  } else {
    carData <- rbind(carData, newData)
  }
}

write.csv(carData, file = "chileautosData.csv")
