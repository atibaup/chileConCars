# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 

library(shiny)
source("scrapingLib.R")
load("data/fittedLmer.RData")
load("data/fittedKmVsAge.RData")

yapoData <- read.csv("data/yapoData.csv")
yapoData$X <- NULL

chileautosData <- read.csv("data/chileautosData.csv")
chileautosData$X <- NULL

GLOBAL.CACHE <- rbind(yapoData, chileautosData)

getYapoCarsByFilter <- function(filter = list(), modelInfos = yapoBrandModelCodes, updateProgress = NULL) {
  yapoCarData <- NULL
  for (modelInfo in modelInfos) {
    if (is.function(updateProgress)) {
      text <- sprintf("Fetching Yapo data for %s %s", modelInfo$make, modelInfo$model)
      updateProgress(detail = text)
    }
    newData <- getYapoCarsDataFrameForModel(modelInfo, filter, cache = GLOBAL.CACHE)
    yapoCarData <- assignOrRbind(yapoCarData, newData)
  }
  if (length(yapoCarData) > 0) {
    return(yapoCarData)
  } else {
    return(NULL)
  }
}

getChileautosCarsByFilter <- function(filter = list(), modelInfos = chileautosBrandModelCodes, updateProgress = NULL) {
  chileautosCarData <- NULL
  for (modelInfo in chileautosBrandModelCodes) {
    if (is.function(updateProgress)) {
      text <- sprintf("Fetching Chileautos data for %s %s", modelInfo$make, modelInfo$model)
      updateProgress(detail = text)
    }    
    newData <- getChileautosCarsDataFrameForModel(modelInfo, filter, cache = GLOBAL.CACHE)
    chileautosCarData <- assignOrRbind(chileautosCarData, newData)
  }
  if (length(chileautosCarData) > 0) {
    return(chileautosCarData)
  } else {
    NULL
  }
}

getCarsByBudget <- function(filter, updateProgress = NULL) {
  
  yapoData <- getYapoCarsByFilter(filter, updateProgress = updateProgress)
  chileautosCarData <- getChileautosCarsByFilter(filter, updateProgress = updateProgress)
  
  if (!is.null(yapoData) & !is.null(chileautosCarData)) {
    carData <- rbind(yapoData, chileautosCarData)
  } else if (!is.null(yapoData)) {
    carData <- yapoData
  } else if (!is.null(chileautosData)) {
    carData <- chileautosCarData
  } else {
    carData <- NULL
  }
  return(carData)
}

getCarsByBudgetRanked <- function(filter, fitted.lmer, updateProgress = NULL) {
  carsByBudget <- getCarsByBudget(filter, updateProgress)
  if (!is.null(carsByBudget)) {
    # Impute mileages that are missing or aberrant
    missingOrAberrant <- with(carsByBudget, is.na(Kilómetros) | Kilómetros == 0)
    carsByBudget$Kilómetros.miles[missingOrAberrant] <- predict(fit.km.vs.age, newdata = subset(carsByBudget, missingOrAberrant))
    carsByBudget$Precio.USD.Predicted <- 10^predict(fitted.lmer, newdata = carsByBudget, allow.new.levels = TRUE)
    createURL.link = function(rowID) {
      sprintf("<a href=%s target=\"_blank\">%s %s</a>", carsByBudget[rowID, ]$URL, carsByBudget[rowID, ]$make, carsByBudget[rowID, ]$model)
    }
    carsByBudget$URL.link = sapply(rownames(carsByBudget), createURL.link)
    #btsrp <- bootMer(fitted.lmer, function(x) 10^predict(x, newdata = carsByBudget), nsim = 100)
    #carsByBudget$Precio.USD.Predicted1stQ <- apply(btsrp$t, 2, function(x) quantile(x, 0.25, na.rm=TRUE))
    carsByBudget$Delta <- round(carsByBudget$Precio.USD.Predicted - carsByBudget$Precio.USD)
    carsByBudget$Precio.M.Pesos <- carsByBudget$Precio / 10^6
    
    sortedCarsByBudget <- carsByBudget[with(carsByBudget, order(-Delta)), ]
    return(sortedCarsByBudget)
  } else {
    return(NULL)
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$table <- renderDataTable({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Fetching data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    filter <- list(maxPrice = as.numeric(input$range[2]) / CHP2USD, 
                   minPrice = as.numeric(input$range[1]) / CHP2USD,
                   maxKm = as.numeric(input$kmRange[2]))
    
    ranking <- getCarsByBudgetRanked(filter, 
                                     fit.mem3, 
                                     updateProgress = updateProgress)
    
    ranking <- subset(ranking, Año >= as.numeric(input$yearRange[1]) & Año <= as.numeric(input$yearRange[2]))
    ranking <- subset(ranking, Kilómetros >= as.numeric(input$kmRange[1]) & Kilómetros <= as.numeric(input$kmRange[2]))
    
    subset <- ranking[, c( "URL.link", "Año", "Kilómetros", "Precio.USD", "Precio.M.Pesos", "Nombre", "Fecha", "Delta")]
    colnames(subset) <- c("URL", "Año", "Km", "USD", "Pesos (Millones)", "Descripción", "Publicado", "Delta")
    subset
  }, escape = FALSE)
})