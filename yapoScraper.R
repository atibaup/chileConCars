#!/usr/bin/env Rscript
'Usage:
   yapoScraper.R [--update] [-f <data folder> -o <output filename>]

Copyright 2016 - Arnau Tibau-Puig
This program is distributed under a GNU General Public License 

Options:
  -o <output filename> Output file [default: yapoData.csv]
  -f <data folder> Output folder [default: data] 
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
for (modelInfo in yapoBrandModelCodes) {
  print(sprintf("Fetching data for %s %s", modelInfo$make, modelInfo$model))
  newData <- getYapoCarsDataFrameForModel(modelInfo, cache = existingData)
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
