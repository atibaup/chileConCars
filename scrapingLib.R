# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License  
require(rvest)
require(RCurl)

MOZILLA.MAC.USER.AGENT <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8)",
                             "AppleWebKit/534.30 (KHTML, like Gecko)", 
                             "Chrome/12.0.742.112 Safari/534.30", sep='')

# 
# Source proxy settings. A list with entries:
#
# proxy.opts <- list(
#   proxy         = "123.456.789.012", 
#   proxyport     = 8080,
#   proxyusername = "user",
#   proxypassword = "password"
# )
source("proxySettings.R")

CHP2USD = 0.00152898
SLEEP.TIME <- 0.5

getHtmlDocument <- function(url, userAgent = MOZILLA.MAC.USER.AGENT) {
  encodedUrl <- URLencode(url)
  h <- basicHeaderGatherer()
  html <- getURL(encodedUrl,
                 .opts = proxy.opts,
                 httpheader = c('User-Agent' = userAgent),
                 headerfunction = h$update)
  if (h$value()["status"] == "200") {
    return(read_html(html))
  } else {
    warning(sprintf("Trying to fetch %s returned status: %d", encodedUrl, h$value()$status))
  }
}

cleanString <- function(x) {
  return(gsub("[$\r\n\t]", "", x))
}

cleanNumericString <- function(x) {
  return(as.numeric(gsub("[_.]", "", cleanString(x))))
}

getCurrentYear <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

assignOrRbind <- function(oldData, newData) {
  if (is.null(oldData)) {
    oldData <- newData
  } else if (!is.null(newData)){
    oldData <- rbind(oldData, newData)
  }
  return(oldData)
}

divideBy1000IfPossible <- function(x) {
  if(is.numeric(x)){ return(x / 1000); } else{ return(NA); }
}

carFields <- c("Año", "Código", "Combustible", "Fecha", "Kilómetros", "Nombre", "Precio",
               "Precio.anterior", "Tipo.de.vehículo", "Transmisión", "URL") 

#
# Yapo Scraper
#

getYapoURLList <- function(yapoResultPageUrl) { 
  resultPageHtml <- getHtmlDocument(yapoResultPageUrl) 
  yapoWarning <- resultPageHtml %>% html_nodes(".redwarning_failover") %>% html_text()
  if (length(yapoWarning) > 0) {
    return(NULL)
  } else {
    yapoCarUrls <- resultPageHtml %>% html_nodes(".redirect-to-url") %>% html_attr("href")
    uniqueCarUrls <- unique(yapoCarUrls)
    return(uniqueCarUrls)
  }
}

fetchYapoCarFeatures <- function(yapoCarPageUrl) {
  carPageHtml <- getHtmlDocument(yapoCarPageUrl) 
  yapoCarFeatures <- carPageHtml %>% html_node("table") %>% html_table()
  yapoCarTimePosted <- carPageHtml %>% html_node("time") %>% html_attr("datetime")
  yapoCarDateMatch <- grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", yapoCarTimePosted, value = TRUE)
  if (length(yapoCarDateMatch) > 0) {
    yearMonthDay <- strsplit(substr(yapoCarDateMatch, 1, 10), "-")[[1]]
    yapoCarDate <- sprintf("%s-%s-%s", yearMonthDay[3], yearMonthDay[2], yearMonthDay[1])
  } else {
    yapoCarDate <- NA
  }
  yapoCarName <- carPageHtml %>% html_node(".title-details") %>% html_text()
  # This is to throttle the requests so as to not make anyone angry
  Sys.sleep(SLEEP.TIME)
  rownames(yapoCarFeatures) <- yapoCarFeatures$X1
  yapoCarFeatures$X1 <- NULL
  colnames(yapoCarFeatures) <- NULL
  carTable <- data.frame(Nombre = cleanString(yapoCarName),
                         Año = cleanNumericString(yapoCarFeatures["Año", ]),
                         Fecha = yapoCarDate,
                         URL = strsplit(yapoCarPageUrl, "\\?")[[1]][1],
                         Código = cleanNumericString(yapoCarFeatures["Código", ]),
                         Kilómetros = cleanNumericString(yapoCarFeatures["Kilómetros", ]),
                         Precio = cleanNumericString(yapoCarFeatures["Precio", ]),
                         Precio.anterior =  cleanNumericString(yapoCarFeatures["Precio anterior", ]),
                         Tipo.de.vehículo = cleanString(yapoCarFeatures["Tipo de vehículo", ]),
                         Combustible = cleanString(yapoCarFeatures["Combustible", ]),
                         Transmisión = cleanString(yapoCarFeatures["Transmisión (cambio)", ]),
                         stringsAsFactors = FALSE)
  if(!all(colnames(carTable) %in% carFields)) {
    stop("Missing fields!")
  }
  return(carTable)
}

getCleanYapoCarTable <- function(url) {
  carFeatures <- try(fetchYapoCarFeatures(url))
  if (class(carFeatures) != "try-error") {
    return(carFeatures)
  } else {
    return(NULL)
  }
}

getYapoCarFeatures <- function(url, cache) {
  if(!is.null(cache)) {
    cleanUrl <- strsplit(url, "\\?")[[1]][1]
    matchInCache <- with(cache, which(URL == cleanUrl))
    if (length(matchInCache) > 0) {
      print(sprintf("Cache hit for %s", url))
      carTable <- cache[matchInCache, carFields]
      return(carTable)
    }
  }
  return(getCleanYapoCarTable(url))
}

getYapoCarsDataFrame <- function(yapoResultPageUrl, maxResults = NULL, cache = NULL) {
  yapoUrlList <- getYapoURLList(yapoResultPageUrl)
  if (length(yapoUrlList) == 0) {
    print(sprintf("%s returned 0 results", yapoResultPageUrl))
    return(NULL)
  }
  if (is.null(maxResults)) {
    yapoUrlListFiltered <- yapoUrlList
  } else {
    yapoUrlListFiltered <- yapoUrlList[1:maxResults]
  }
  carsDataFrame <- NULL
  numUrl <- 0 
  for (url in yapoUrlListFiltered) {
    print(sprintf("Fetching %s (%d)", url, numUrl))
    carFeatures <- try(getYapoCarFeatures(url, cache))
    if (class(carFeatures) != 'try-error'){
      carsDataFrame <- assignOrRbind(carsDataFrame, carFeatures)
    }
    numUrl = numUrl + 1
  }
  return(carsDataFrame)
}

addColumns <- function(carsDataFrame, chpToUsd) {
  # Additional columns
  currentYear = as.integer(format(Sys.Date(), "%Y"))
  carsDataFrame$Edad = sapply(carsDataFrame$Año, function(x) currentYear - x)
  carsDataFrame$Precio.USD = sapply(carsDataFrame$Precio, function(x) round(x * chpToUsd))
  carsDataFrame$Kilómetros.miles = sapply(carsDataFrame$Kilómetros, divideBy1000IfPossible)
  return(carsDataFrame)
}

getYapoCleanCarsDataframe <- function(yapoResultPageUrl, maxResults = NULL, chpToUsd = CHP2USD, cache = NULL) {
  carsDataFrame <- getYapoCarsDataFrame(yapoResultPageUrl, maxResults, cache)
  if (length(carsDataFrame) > 0) {
    return(addColumns(carsDataFrame, chpToUsd))
  } else {
    return(NULL)
  }
}

getYapoCarsDataFrameForModel <- function(modelInfo, filter = list(minPrice = NULL, maxPrice = NULL, maxKm = NULL), cache = NULL) {
  getListUrl <- function(pageNumber) {
    yapoPriceCategories <- c(0, 250, 500 * (1:50)) * 1000
    if (is.null(filter$minPrice)) {
      minPrice <- NULL
    } else {
      minPrice <- which.min(abs(yapoPriceCategories - filter$minPrice))
    }
    if (is.null(filter$maxPrice)) {
      maxPrice <- NULL
    } else {
      maxPrice <- which.min(abs(yapoPriceCategories - filter$maxPrice))
    }
    yapoListBaseUrl <- "http://www.yapo.cl/region_metropolitana/autos?ca=15_s&l=1&w=1&cmn=&st=s&br=%d&mo=%d&o=%d"
    url = sprintf(yapoListBaseUrl, modelInfo$makeCode, modelInfo$modelCode, pageNumber)
    if (!is.null(minPrice)) {
      url = paste(url, sprintf("&ps=%d", minPrice), sep = '')
    }
    if (!is.null(maxPrice)) {
      url = paste(url, sprintf("&pe=%d", maxPrice), sep = '')
    }
    if (!is.null(filter$maxKm)) {
      url = paste(url, sprintf("&me=%d", filter$maxKm), sep = '')
    }
    print(sprintf("List URL: %s", url))
    return(url)
  }
  pageNumber <- 1
  carData <- NULL
  newPageData <- getYapoCleanCarsDataframe(getListUrl(pageNumber), cache = cache)
  while (length(newPageData) > 0) {
    pageNumber <- pageNumber + 1 
    carData <- assignOrRbind(carData, newPageData)
    newPageData <- getYapoCleanCarsDataframe(getListUrl(pageNumber), cache = cache)
  }
  if (!is.null(carData)) {
    carData$make <- modelInfo$make
    carData$model <- modelInfo$model
  }
  return(carData)
}

#
# chileautos Scraper
#

getChileautosURLList <- function(chileautosResultPageUrl) { 
  resultPageHtml <- getHtmlDocument(chileautosResultPageUrl) 
  caCarUrls <- resultPageHtml %>% html_nodes("#results :nth-child(1)") %>% html_nodes("a") %>% html_attr("href")
  uniqueCarUrls <- unique(sapply(caCarUrls, function(x) sprintf("http:%s", x)))
  return(uniqueCarUrls)
}

fetchChileautosCarFeatures <- function(caCarPageUrl) {
  carPageHtml <- getHtmlDocument(caCarPageUrl) 
  title <- NA
  km <- NA
  year <- NA
  transmission <- NA
  fuel <- NA
  rows <- carPageHtml %>% html_nodes("tr")
  for (row in rows) {
    cols <- row %>% html_nodes("td")
    name <- try(cols %>% .[[1]] %>% html_text(), silent = TRUE)
    value <- try(cols %>% .[[2]] %>% html_text(), silent = TRUE)
    if (class(name) != 'try-error' & class(value) != 'try-error') {
      if (name == "Versión:") {
        title <- cleanString(value)
      } else if(name == "Kilometraje:") {
        km <- cleanNumericString(cleanString(value))
      } else if (name == "Año:") {
        year <- cleanNumericString(cleanString(value))
      } else if (name == "Transmisión:") {
        transmission <- cleanString(value)
      } else if (name == "Combustible") {
        fuel <- cleanString(value)
      }
    }
  }
  parseDate <- function(s) {
    match <- grep("Publicado [0-9]{2}-[0-9]{2}-[0-9]{2}", s, value = TRUE)
    if (length(match) > 0) {
      parsedDate <- substr(strsplit(match, "Publicado ")[[1]][2], 1, 10)
      return(parsedDate)
    } else {
      return(NA)
    }
  }
  fecha <- parseDate(carPageHtml %>% html_text())
  price <- carPageHtml %>% html_nodes("b") %>% .[[2]] %>% html_text()
  cleanPrice <- cleanNumericString(cleanString(price))
  codigo <- as.numeric(strsplit(caCarPageUrl, "codauto=")[[1]][2])
  # Standardizing
  if (!is.na(fuel)) {
    if (fuel == 'Diesel (Petroleo)') {
      cleanFuel <- 'Diesel'
    } else {
      cleanFuel <- fuel
    }
  } else {
    cleanFuel <- NA
  }
  if (!is.na(transmission)) {
    if (transmission == 'Automática') {
      cleanTransmission <- 'Automático'
    } else {
      cleanTransmission <- transmission
    }
  } else {
    cleanTransmission <- NA
  }
  carTable <- data.frame(Nombre = cleanString(title),
                         Año = cleanNumericString(year),
                         Fecha = fecha,
                         URL = caCarPageUrl,
                         Código = codigo,
                         Kilómetros = km,
                         Precio = cleanPrice,
                         Precio.anterior =  NA,
                         Tipo.de.vehículo = NA,
                         Combustible = cleanFuel,
                         Transmisión = cleanTransmission,
                         stringsAsFactors = FALSE)
  if(!all(colnames(carTable) %in% carFields)) {
    stop("Missing fields!")
  }
  return(carTable)
  Sys.sleep(SLEEP.TIME)
  return(carTable)
}

getChileautosCarFeatures <- function(url, cache) {
  if(!is.null(cache)) {
    matchInCache <- with(cache, which(URL == url))
    if (length(matchInCache) > 0) {
      print(sprintf("Cache hit for %s", url))
      carTable <- cache[matchInCache, carFields]
      return(carTable)
    }
  }
  return(fetchChileautosCarFeatures(url))
}

getChileautoCarsDataFrame <- function(caResultPageUrl, maxResults = NULL, cache = NULL) {
  caUrlList <- getChileautosURLList(caResultPageUrl)
  if (length(caUrlList) == 0) {
    print(sprintf("%s returned 0 results", caResultPageUrl))
    return(NULL)
  }
  if (is.null(maxResults)) {
    caUrlListFiltered <- caUrlList
  } else {
    caUrlListFiltered <- caUrlList[1:maxResults]
  }
  carsDataFrame <- NULL
  numUrl <- 0 
  for (url in caUrlListFiltered) {
    print(sprintf("Fetching %s (%d)", url, numUrl))
    carFeatures <- try(getChileautosCarFeatures(url, cache))
    if (class(carFeatures) != "try-error") {
      carsDataFrame <- assignOrRbind(carsDataFrame, carFeatures)
    }
    numUrl = numUrl + 1
  }
  return(carsDataFrame)
}

getCleanChileautosCarsDataframe <- function(caResultPageUrl, maxResults = NULL, chpToUsd = CHP2USD, cache = NULL) {
  carsDataFrame <- getChileautoCarsDataFrame(caResultPageUrl, maxResults, cache)
  if (length(carsDataFrame) > 0) {
    return(addColumns(carsDataFrame, chpToUsd))
  } else {
    return(NULL)
  }
}

getChileautosCarsDataFrameForModel <- function(modelInfo, filter = list(minPrice = NULL, maxPrice = NULL, maxKm = NULL), cache = NULL) {
  getListUrl <- function(pageNumber) {
    chileautosListBaseUrl <- "http://www.chileautos.cl/cemagic.asp?disp=1&goo=0&sort=fd&region=13&maresp=%d&modelo=%s&pag=%d"
    url = sprintf(chileautosListBaseUrl, modelInfo$makeCode, modelInfo$modelCode, pageNumber)
    if (!is.null(filter$minPrice)) {
      url = paste(url, sprintf("&pi=%d", floor(filter$minPrice)), sep = '')
    }
    if (!is.null(filter$maxPrice)) {
      url = paste(url, sprintf("&pf=%d", ceiling(filter$maxPrice)), sep = '')
    }
    if (!is.null(filter$maxKm)) {
      url = paste(url, sprintf("&kilom=%d", filter$maxKm), sep = '')
    }
    print(sprintf("List URL: %s", url))
    return(url)
  }
  pageNumber <- 1
  carData <- NULL
  newPageData <- getCleanChileautosCarsDataframe(getListUrl(pageNumber), cache = cache)
  while (pageNumber < 100) { #safegard
    pageNumber <- pageNumber + 1 
    carData <- assignOrRbind(carData, newPageData)
    lastPageData <- newPageData
    newPageData <- getCleanChileautosCarsDataframe(getListUrl(pageNumber), cache = cache)
    # Chileautos returns last results even if page number is larger than largest page number.
    # This hack stops the page number addition once the results are the same as for the last page number.
    if (length(newPageData$URL) == 0) {
      break
    }
    if (length(lastPageData$URL) == length(newPageData$URL)) {
      if (all(lastPageData$URL == newPageData$URL)) {
        break
      }
    }
  }
  if (!is.null(carData)) {
    carData$make <- modelInfo$make
    carData$model <- modelInfo$model
  }
  return(carData)
}

#
# Brand/model to website codes
#

yapoBrandModelCodes <- list(list(make = 'Toyota', model = '4Runner', makeCode = 88, modelCode = 1),
                            list(make = 'Toyota', model = 'LandCruiser', makeCode = 88, modelCode = 15),
                            list(make = 'Nissan', model = 'Pathfinder', makeCode = 64, modelCode = 30),
                            list(make = 'Nissan', model = 'X-Trail', makeCode = 64, modelCode = 45),
                            list(make = 'Mitsubishi', model = 'Montero', makeCode = 62, modelCode = 11),
                            list(make = 'Suzuki', model = 'GrandVitara', makeCode = 86, modelCode = 15),
                            list(make = 'Suzuki', model = 'GrandNomade', makeCode = 86, modelCode =14),
                            list(make = 'Honda', model = 'CR-V', makeCode = 39, modelCode = 5))

chileautosBrandModelCodes <- list(list(make = 'Toyota', model = '4Runner', makeCode = 4, modelCode = '4Runner'),
                                  list(make = 'Toyota', model = 'LandCruiser', makeCode = 4, modelCode = 'Land Cruiser'),
                                  list(make = 'Nissan', model = 'Pathfinder', makeCode = 17, modelCode = 'Pathfinder'),
                                  list(make = 'Nissan', model = 'X-Trail', makeCode = 17, modelCode = 'X-Trail'),
                                  list(make = 'Mitsubishi', model = 'Montero', makeCode = 3, modelCode = 'Montero'),
                                  list(make = 'Suzuki', model = 'GrandVitara', makeCode = 9, modelCode = 'Grand Vitara'),
                                  list(make = 'Suzuki', model = 'GrandNomade', makeCode = 9, modelCode = 'Grand Nomade'),
                                  list(make = 'Honda', model = 'CR-V', makeCode = 21, modelCode = 'CRV'))

