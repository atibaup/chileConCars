# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 
source("scrapingLib.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("ChileConCars - Copyright Arnau Tibau-Puig 2016"),
  
  sidebarLayout(
    sidebarPanel(
      #textInput('budget', "Input your budget in USD", value = "5000"),
      sliderInput("range", 
                  label = "Budget range:",
                  min = 0, max = 15000, value = c(7000, 8000),
                  step = 500, round = TRUE),
      sliderInput("kmRange", 
                  label = "Kilometers range:",
                  min = 0, max = 500000, value = c(100000, 200000),
                  step = 10000, round = TRUE),
      sliderInput("yearRange", 
                  label = "Year range:",
                  min = 1990, max = getCurrentYear(), value = c(2000, 2010),
                  step = 1, round = TRUE),
      submitButton("Get cars!")
    ),
    
    mainPanel(
      dataTableOutput('table')
    )
  )
))