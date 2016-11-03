#!/usr/bin/env Rscript
'Usage:
   chileautosScraper.R [-f <data folder> -o <output filename> --update]

Copyright 2016 - Arnau Tibau-Puig
This program is distributed under a GNU General Public License 

Options:
  -o Output file [default: chileautosData.csv]
  -f Output folder [default: data] 
  -u Update only [default: FALSE]
 ]' -> doc

require(docopt)
source("scrapingLib.R")

opts <- docopt(doc)

dataFolder <- opts$f
dataFile <- paste(dataFolder, "/", opts$o, sep = '')

if (opts$update & file.exists(dataFile)) {
  print(sprintf("Running in update mode, using data in %s", dataFile))
  existingData <- read.csv(dataFile, stringsAsFactors = FALSE)
} else {
  existingData <- NULL
}

carData <- NULL
for (modelInfo in chileautosBrandModelCodes) {
  print(sprintf("Fetching data for %s %s", modelInfo$make, modelInfo$model))
  newData <- getChileautosCarsDataFrameForModel(modelInfo, cache = existingData)
  if (is.null(carData)) {
    carData <- newData
  } else {
    carData <- rbind(carData, newData)
  }
}

if (!dir.exists(dataFolder)) {
  dir.create(dataFolder)
}
write.csv(carData, file = dataFile)
