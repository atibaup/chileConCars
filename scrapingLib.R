# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License  
require(rvest)
require(curl)

mozillaMacUserAgent <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8)",
                             "AppleWebKit/534.30 (KHTML, like Gecko)", 
                             "Chrome/12.0.742.112 Safari/534.30", sep='')

currentChpToUsd = 0.00152898

getHtmlDocument <- function(url, userAgent = mozillaMacUserAgent) {
  h <- new_handle()
  handle_setheaders(h, "User-Agent" = userAgent)
  encodedUrl <- URLencode(url)
  req <- curl_fetch_memory(encodedUrl, handle=h)
  if (req$status_code == 200) {
    html <- read_html(req$content)
    handle_reset(h)
    return(html)
  } else {
    warning(paste("Trying to fetch", encodedUrl, "returned status:", req$status_code))
  }
}

cleanString <- function(x) {
  return(gsub("[$\r\n\t]", "", x))
}

cleanNumericString <- function(x) {
  return(gsub("[_.]", "", x))
}

#
# Yapo Scraper
#

getYapoURLList <- function(yapoResultPageUrl) { 
  resultPageHtml <- getHtmlDocument(yapoResultPageUrl) 
  yapoCarUrls <- resultPageHtml %>% html_nodes(".redirect-to-url") %>% html_attr("href")
  uniqueCarUrls <- unique(yapoCarUrls)
  return(uniqueCarUrls)
}

getYapoCarFeatures <- function(yapoCarPageUrl) {
  carPageHtml <- getHtmlDocument(yapoCarPageUrl) 
  yapoCarFeatures <- carPageHtml %>% html_node("table") %>% html_table()
  yapoCarTimePosted <- carPageHtml %>% html_node("time") %>% html_attr("datetime")
  yapoCarName <- carPageHtml %>% html_node(".title-details") %>% html_text()
  carTable <- rbind(yapoCarFeatures, c('Fecha', yapoCarTimePosted))
  carTable <- rbind(carTable, c('Nombre', yapoCarName))
  carTable <- rbind(carTable, c('URL', yapoCarPageUrl))
  colnames(carTable) <- c('Propiedad', 'Valor')
  rownames(carTable) <- carTable$Propiedad
  carTable$Propiedad <- NULL
  return(carTable)
}

yapoFields <- c("Año", "Código", "Combustible", "Fecha", "Kilómetros", "Nombre", "Precio", "Edad",
               "Precio.anterior", "Precio.USD", "Tipo", "Tipo.de.vehículo", "Transmisión..cambio.", "URL")                  

yapoNumFields <- c("Precio", "Precio.USD", "Año", "Kilómetros", "Código")

cleanYapoCarTable <- function(carTable) {
  carTable$Valor <- sapply(carTable$Valor, cleanString)
  isNumeric <- rownames(carTable) %in% yapoNumFields
  carTable$Valor[isNumeric] = sapply(carTable$Valor[isNumeric], cleanNumericString)
  return(carTable)
}

getYapoCarsDataFrame <- function(yapoResultPageUrl, maxResults = NULL, sleepTime = 1) {
  yapoUrlList <- getYapoURLList(yapoResultPageUrl)
  if (length(yapoUrlList) == 0) {
    warning(sprintf("%s returned 0 results, skipping", yapoResultPageUrl))
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
    carFeatures <- try(getYapoCarFeatures(url))
    if (class(carFeatures) != "try-error") {
      cleanedCarFeatures <- cleanYapoCarTable(carFeatures)
      if (is.null(carsDataFrame)) {
        carsDataFrame <- cleanedCarFeatures
      } else {
        carsDataFrame <- merge(carsDataFrame, 
                               cleanedCarFeatures, 
                               all.x = TRUE,
                               all.y = TRUE,
                               by = "row.names",
                               suffixes = c('', numUrl + 1))
        rownames(carsDataFrame) = carsDataFrame$Row.names
        carsDataFrame$Row.names <- NULL
      }
    }
    numUrl = numUrl + 1
    Sys.sleep(sleepTime)
  }
  carsDataFrame <- carsDataFrame[, !colnames(carsDataFrame) %in% "Propiedad"]
  colnames(carsDataFrame) <- NULL
  tCarsDataFrame = data.frame(t(carsDataFrame), stringsAsFactors = FALSE)
  return(tCarsDataFrame)
}

cleanYapoCarsDataframe <- function(carsDataFrame, chpToUsd) {
  # Adding columns that may be missing so that we can rbind across models
  for (name in yapoFields) {
    if (!name %in% names(carsDataFrame)){
      carsDataFrame[, name] = NA
    }
  }
  # Converting numeric columns from string
  for (name in yapoNumFields) {
    carsDataFrame[, name] = as.numeric(carsDataFrame[, name])
  }
  # Additional columns
  currentYear = as.integer(format(Sys.Date(), "%Y"))
  carsDataFrame$Edad = sapply(carsDataFrame$Año, function(x) currentYear - x)
  carsDataFrame$Precio.USD = sapply(carsDataFrame$Precio, function(x) x * chpToUsd)
  return(carsDataFrame)
}

getYapoCleanCarsDataframe <- function(yapoResultPageUrl, maxResults = NULL, sleepTime = 1, chpToUsd = currentChpToUsd) {
  carsDataFrame <- getYapoCarsDataFrame(yapoResultPageUrl, maxResults, sleepTime)
  return(cleanYapoCarsDataframe(carsDataFrame, chpToUsd))
}

getYapoCarsDataFrameForModel <- function(modelInfo) {
  getListUrl <- function(pageNumber) {
    yapoListBaseUrl <- "http://www.yapo.cl/region_metropolitana/autos?ca=15_s&l=1&w=1&cmn=&st=s&br=%d&mo=%d&o=%d"
    url = sprintf(yapoListBaseUrl, modelInfo$makeCode, modelInfo$modelCode, pageNumber)
    print(sprintf("List URL: %s", url))
    return(url)
  }
  pageNumber <- 1
  carData <- NULL
  newPageData <- getYapoCleanCarsDataframe(getListUrl(pageNumber))
  while (length(newPageData) > 0) {
    pageNumber <- pageNumber + 1 
    if (is.null(carData)) {
      carData <- newPageData
    } else {
      carData <- rbind(carData, newPageData)
    }
    newPageData <- getYapoCleanCarsDataframe(getListUrl(pageNumber))
  }
  carData$make <- modelInfo$make
  carData$model <- modelInfo$model
  return(carData)
}

#
# chileautos Scraper
#

getChileautosURLList <- function(chileautosResultPageUrl) { 
  resultPageHtml <- getHtmlDocument(chileautosResultPageUrl) 
  caCarUrls <- resultPageHtml %>% html_nodes("#results :nth-child(1)") %>% html_nodes("a") %>% html_attr("href")
  uniqueCarUrls <- unique(sapply(caCarUrls, function(x) substring(x, 3)))
  return(uniqueCarUrls)
}

getChileautosCarFeatures <- function(caCarPageUrl) {
  carPageHtml <- getHtmlDocument(caCarPageUrl) 
  rows <- carPageHtml %>% html_nodes("tr")
  title <- NA
  km <- NA
  year <- NA
  transmission <- NA
  fuel <- NA
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
  price <- carPageHtml %>% html_nodes("b") %>% .[[2]] %>% html_text()
  cleanPrice <- cleanNumericString(cleanString(price))
  carTable <- data.frame(Valor = c(title, km, year, cleanPrice, caCarPageUrl, transmission, fuel), 
                         Propiedad = c("Nombre", "Kilómetros", "Año", "Precio", "URL", "Transmisión..cambio.", "Combustible"),
                         row.names = 2,
                         stringsAsFactors = FALSE)
  return(carTable)
}

getChileautoCarsDataFrame <- function(caResultPageUrl, maxResults = NULL, sleepTime = 1) {
  caUrlList <- getChileautosURLList(caResultPageUrl)
  if (length(caUrlList) == 0) {
    warning(sprintf("%s returned 0 results, skipping", caResultPageUrl))
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
    carFeatures <- try(getChileautosCarFeatures(url))
    if (class(carFeatures) != "try-error") {
      if (is.null(carsDataFrame)) {
        carsDataFrame <- carFeatures
      } else {
        carsDataFrame <- merge(carsDataFrame, 
                               carFeatures, 
                               all.x = TRUE,
                               all.y = TRUE,
                               by = "row.names",
                               suffixes = c('', numUrl + 1))
        rownames(carsDataFrame) = carsDataFrame$Row.names
        carsDataFrame$Row.names <- NULL
      }
    }
    numUrl = numUrl + 1
    Sys.sleep(sleepTime)
  }
  carsDataFrame <- carsDataFrame[, !colnames(carsDataFrame) %in% "Propiedad"]
  colnames(carsDataFrame) <- NULL
  tCarsDataFrame = data.frame(t(carsDataFrame), stringsAsFactors = FALSE)
  return(tCarsDataFrame)
}

chileautosNumFields <- c("Año", "Kilómetros", "Precio", "Precio.USD")

cleanCaCarsDataframe <- function(carsDataFrame, chpToUsd) {
  # Adding columns that may be missing so that we can rbind across models and sites
  for (name in yapoFields) {
    if (!name %in% names(carsDataFrame)){
      carsDataFrame[, name] = NA
    }
  }
  # Converting numeric columns from string
  for (name in chileautosNumFields) {
    carsDataFrame[, name] = as.numeric(carsDataFrame[, name])
  }
  # Additional columns
  currentYear = as.integer(format(Sys.Date(), "%Y"))
  carsDataFrame$Edad = sapply(carsDataFrame$Año, function(x) currentYear - x)
  carsDataFrame$Precio.USD = sapply(carsDataFrame$Precio, function(x) x * chpToUsd)
  return(carsDataFrame)
}

getCleanChileautosCarsDataframe <- function(caResultPageUrl, maxResults = NULL, sleepTime = 0.5, chpToUsd = currentChpToUsd) {
  carsDataFrame <- getChileautoCarsDataFrame(caResultPageUrl, maxResults, sleepTime)
  return(cleanCaCarsDataframe(carsDataFrame, chpToUsd))
}

getChileautosCarsDataFrameForModel <- function(modelInfo) {
  getListUrl <- function(pageNumber) {
    chileautosListBaseUrl <- "http://www.chileautos.cl/cemagic.asp?disp=1&goo=0&sort=fd&region=13&maresp=%d&modelo=%s&pag=%d"
    url = sprintf(chileautosListBaseUrl, modelInfo$makeCode, modelInfo$modelCode, pageNumber)
    print(sprintf("List URL: %s", url))
    return(url)
  }
  pageNumber <- 1
  carData <- NULL
  newPageData <- getCleanChileautosCarsDataframe(getListUrl(pageNumber))
  while (pageNumber < 100) { #safegard
    pageNumber <- pageNumber + 1 
    if (is.null(carData)) {
      carData <- newPageData
    } else {
      carData <- rbind(carData, newPageData)
    }
    lastPageData <- newPageData
    newPageData <- getCleanChileautosCarsDataframe(getListUrl(pageNumber))
    # Chileautos returns last results even if page number is larger than largest page number.
    # This hack stops the page number addition once the results are the same as for the last page number.
    if (length(lastPageData$URL) == length(newPageData$URL)) {
      if (all(lastPageData$URL == newPageData$URL)) {
        break
      }
    }
  }
  carData$make <- modelInfo$make
  carData$model <- modelInfo$model
  return(carData)
}
