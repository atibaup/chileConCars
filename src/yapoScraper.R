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

if (!dir.exists(dataFolder)) {
  dir.create(dataFolder)
}

dataFile <- paste(dataFolder, "/", opts$o, sep = '')

if (opts$update & file.exists(dataFile)) {
  print(sprintf("Running in update mode, using data in %s", dataFile))
  existingData <- read.csv(dataFile, stringsAsFactors = FALSE)
} else {
  existingData <- NULL
}

fileNames <- NULL
for (modelInfo in yapoBrandModelCodes) {
  print(sprintf("Fetching data for %s %s", modelInfo$make, modelInfo$model))
  carData <- getYapoCarsDataFrameForModel(modelInfo, filter = list(minPrice = 6500, maxPrice = 7000), cache = existingData)
  modelFileName <- paste(dataFolder, "/", modelInfo$model, ".csv", sep = '')
  write.csv(carData, file = modelFileName)
  fileNames = c(fileNames, modelFileName)
}

# Compile each's model csv and save
carsData <- NULL
for (fileName in fileNames) {
  modelData <- tryCatch(read.csv(fileName, stringsAsFactors = FALSE), error = function(x) NULL)
  if (!is.null(modelData)) {
    if (is.null(carsData)){
      carsData <- modelData
    } else {
      carsData <- rbind(carsData, modelData)
    }
  }
}
write.csv(carsData, file = dataFile)
