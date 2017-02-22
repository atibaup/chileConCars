#!/usr/bin/env Rscript
'Usage:
   yapoImages.R [-f <output folder>] [-i <yapo data>]

Copyright 2016 - Arnau Tibau-Puig
This program is distributed under a GNU General Public License 

Options:
  -f <output folder> Output folder [default: data/images/] 
  -i <input filename> Input file [default: data/yapoData.csv]
 ]' -> doc

require(docopt)
source("scrapingLib.R")

opts <- docopt(doc)

outputFolder <- opts$f

if (!dir.exists(outputFolder)) {
  dir.create(outputFolder)
}

if (file.exists(opts$i)) {
  data <- read.csv(opts$i, stringsAsFactors = FALSE)
} else {
  error("Input File must exist")
}

nImages = 0
for (i in seq(nrow(data))) {
  id <- data[i, ]$Código
  idFolder = paste(outputFolder, "/", id, sep='')
  img.urls = strsplit(data[i, ]$IMG.URLs, ",")[[1]]
  if (length(img.urls) > 0) {
    if (!dir.exists(idFolder)) {
      dir.create(idFolder)
    }
    j = 0
    for (url in img.urls) {
      idFile = paste(idFolder, "/", j, ".jpg", sep='')
      if (!file.exists(idFile)) {
        print(paste("Downloading", img.urls, collapse = ","))
        if (tryCatch(download.file(url, idFile), error = function(x) -1) == 0) {
          nImages = nImages + 1
        }
        Sys.sleep(SLEEP.TIME)
      }
      j = j + 1
    }
  }
}

print(paste("Total of", nImages, "downloaded."))

