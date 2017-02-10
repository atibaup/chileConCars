# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 

source("scrapingLib.R")
require(ggplot2)
require(gridExtra)
require(lme4)
require(splines)
library(merTools)
require(gamm4)
require(lattice)
require(caret)

graphics.off()
rm(list=ls())

cleanCarData <- read.csv('../data/cleanCarData.csv')

cleanCarData$X <- NULL

age.knots = quantile(cleanCarData$Edad, probs = c(1/3, 2/3))
age.bndy = c(0, max(cleanCarData$Edad))

bs.cols = data.frame(bs(cleanCarData$Edad, knots = age.knots, Boundary.knots = age.bndy))
names(bs.cols) = c('bs.1', 'bs.2', 'bs.3', 'bs.4', 'bs.5')

ns.cols = data.frame(ns(cleanCarData$Edad, knots = age.knots, Boundary.knots = age.bndy))
names(ns.cols) = c('ns.1', 'ns.2', 'ns.3')

cleanCarData = cbind(cleanCarData, bs.cols)
cleanCarData = cbind(cleanCarData, ns.cols)

availableModels = unique(cleanCarData$model)

textSize = 4

################################################################################################
#
# price ~ age + other covariates
#
################################################################################################

model.rows = c("Precio.USD", "Transmisión", "Combustible", "Edad")

age.knots = quantile(cleanCarData$Edad, probs = c(1/3, 2/3))
age.bndy = c(0, max(cleanCarData$Edad))

model.names = c('bs.fe', 'bs.me', 'bs.me*', 'gams')
testErrorVsFraction = data.frame(row.names = model.names)
fractions = c(.2, .4, .6, .8, 1)
nRndm = 5
for (fraction in fractions) {
  median.test.error = data.frame(row.names = model.names)
  for (n in 1:nRndm) {
    trainIndx = createDataPartition(cleanCarData$model, p = fraction * 0.9, list = FALSE)
    trainData = cleanCarData[trainIndx, ]
    testData = cleanCarData[-trainIndx, ]
    
    trainData = trainData[complete.cases(trainData[, model.rows]), ]
    testData = testData[complete.cases(testData[, model.rows]), ]
    
    # train global models
    
    fit.mem4 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + bs(Edad, knots = age.knots, Boundary.knots = age.bndy) + 
                      (1 + bs(Edad, knots = age.knots, Boundary.knots = age.bndy) | model), 
                    data=trainData)
    
    fit.mem4.ind = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 + 
                      (1 + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 || model), 
                    data=trainData)
    
    # train model-wise models
    fit.gams = list()
    fit.spline.fe = list()

    test.error = data.frame(row.names = model.names)
    
    for (model_ in availableModels) {
      model.train.data = subset(trainData, model == model_)
      model.test.data = subset(testData, model == model_)
      
      test.mse <- function(fit) {
        predicted = predict(fit, newdata=model.test.data) 
        sqrt(mean((10^predicted - model.test.data$Precio.USD)**2))
      }
      
      fit.gams[[model_]] = tryCatch(gam(log10(Precio.USD) ~ 1 + Transmisión + s(Edad) , data = model.train.data),
                                  error = function(e) tryCatch(gam(log10(Precio.USD) ~ 1 + s(Edad) , data = model.train.data),
                                    error = function(e2) gam(log10(Precio.USD) ~ 1 , data = model.train.data)))
      fit.spline.fe[[model_]] = tryCatch(lm(log10(Precio.USD) ~ 1 + Transmisión + bs(Edad, knots = age.knots, Boundary.knots = age.bndy), data = model.train.data),
                                         error = function(e) lm(log10(Precio.USD) ~ 1 + bs(Edad, knots = age.knots, Boundary.knots = age.bndy), data = model.train.data))

      model.test.error = data.frame(c(
                                      test.mse(fit.spline.fe[[model_]]),
                                      test.mse(fit.mem4),
                                      test.mse(fit.mem4.ind),
                                      test.mse(fit.gams[[model_]])),
                                      row.names = model.names)
      colnames(model.test.error) <- model_
      test.error = cbind(test.error, model.test.error)
    }
    median.test.error = cbind(median.test.error, apply(test.error, 1, median))
  }
  print(sprintf('p = %.2f', fraction))
  median.test.error <- apply(median.test.error, 1, median)
  testErrorVsFraction <- cbind(testErrorVsFraction,  median.test.error)
}

print(testErrorVsFraction)

dev.new(); 
matplot(fractions, t(as.matrix(testErrorVsFraction)), 
                   type = 'o',
                   lty = 1,
                   col = c('red', 'red', 'red', 'pink'),
                   pch = c(15, 19, 25, 15),
                   ylab = 'Test MSE',
                   xlab = 'Fraction of Training Data',
                   ylim = c(2000, 2700))

legend('topright', legend = model.names,
       pch = c(15, 19, 25, 15),
       col = c('red', 'red', 'red', 'pink'))
