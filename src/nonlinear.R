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

availableModels = unique(cleanCarData$model)

model.rows = c("Precio.USD", "Transmisión", "Combustible", "Edad")

age.knots = quantile(cleanCarData$Edad, probs = c(1/3, 2/3))
age.bndy = c(0, max(cleanCarData$Edad))

bs.cols = data.frame(bs(cleanCarData$Edad, knots = age.knots, Boundary.knots = age.bndy))
names(bs.cols) = c('bs.1', 'bs.2', 'bs.3', 'bs.4', 'bs.5')

ns.cols = data.frame(ns(cleanCarData$Edad, knots = age.knots, Boundary.knots = age.bndy))
names(ns.cols) = c('ns.1', 'ns.2', 'ns.3')

cleanCarData = cbind(cleanCarData, bs.cols)
cleanCarData = cbind(cleanCarData, ns.cols)

trainIndx = createDataPartition(cleanCarData$model, p = 0.8, list = FALSE)
trainData = cleanCarData[trainIndx, ]
testData = cleanCarData[-trainIndx, ]

trainData = trainData[complete.cases(trainData[, model.rows]), ]
testData = testData[complete.cases(testData[, model.rows]), ]

fit.fe = list()
fit.gams = list()
fit.spline.fe = list()
fit.ns.fe = list()

model.names = c('fe', 'me', 'bs.fe', 'bs.me', 'bs.me*', 'ns.fe', 'ns.me', 'ns.me*', 'gams')
avg.test.error = data.frame(row.names = c(model.names, 'n.samples'))
avg.train.error = data.frame(row.names = c(model.names, 'n.samples'))
nRndom = 10
for (n in 0:nRndom) {
  print(n)
  trainIndx = createDataPartition(cleanCarData$model, p = 0.8, list = FALSE)
  trainData = cleanCarData[trainIndx, ]
  testData = cleanCarData[-trainIndx, ]
  
  trainData = trainData[complete.cases(trainData[, model.rows]), ]
  testData = testData[complete.cases(testData[, model.rows]), ]
  
  train.error = data.frame(row.names = c(model.names, 'n.samples'))
  test.error = data.frame(row.names = c(model.names, 'n.samples'))
  
  fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + Edad + (1 + Edad | model), trainData)

  fit.mem4 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 + 
                    (1 + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 | model), 
                  data=trainData)
  
  fit.mem4.ind = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 + 
                        (1 + bs.1 + bs.2 + bs.3 + bs.4 + bs.5 || model), 
                      data=trainData)
  
  fit.mem.ns = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + ns.1 + ns.2 + ns.3 + 
                      (1 + ns.1 + ns.2 + ns.3 | model), 
                    data=trainData)
  
  fit.mem.ns.ind = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + ns.1 + ns.2 + ns.3 +
                          (1 + ns.1 + ns.2 + ns.3 || model), 
                        data=trainData)
  
  for (model_ in availableModels) {
      
      model.train.data = subset(trainData, model == model_)
      model.test.data = subset(testData, model == model_)
      
      train.mse <- function(fit) {
        fitted = predict(fit, newdata=model.train.data) 
        sqrt(mean((10^fitted - model.train.data$Precio.USD)**2))
      }
      
      test.mse <- function(fit) {
        predicted = predict(fit, newdata=model.test.data) 
        sqrt(mean((10^predicted - model.test.data$Precio.USD)**2))
      }
      
      fit.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión + Edad, data = model.train.data)
      fit.gams[[model_]] = gam(log10(Precio.USD) ~ 1 + Transmisión + s(Edad) , data = model.train.data)
      fit.spline.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión + bs.1 + bs.2 + bs.3 + bs.4 + bs.5, data = model.train.data)
      fit.ns.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión + ns.1 + ns.2 + ns.3, data = model.train.data)
      
      model.train.error = data.frame(c(train.mse(fit.fe[[model_]]), 
                                       train.mse(fit.mem3),
                                       train.mse(fit.spline.fe[[model_]]),
                                       train.mse(fit.mem4),
                                       train.mse(fit.mem4.ind),
                                       train.mse(fit.ns.fe[[model_]]),
                                       train.mse(fit.mem.ns),
                                       train.mse(fit.mem.ns.ind),
                                       train.mse(fit.gams[[model_]]),
                                       nrow(model.train.data)), 
                                     row.names = c(model.names, 'n.samples'))
      
      model.test.error = data.frame(c(test.mse(fit.fe[[model_]]), 
                                      test.mse(fit.mem3),
                                      test.mse(fit.spline.fe[[model_]]),
                                      test.mse(fit.mem4),
                                      test.mse(fit.mem4.ind),
                                      test.mse(fit.ns.fe[[model_]]),
                                      test.mse(fit.mem.ns),
                                      test.mse(fit.mem.ns.ind),
                                      test.mse(fit.gams[[model_]]),
                                      nrow(model.test.data)), 
                                    row.names = c(model.names, 'n.samples'))
    colnames(model.train.error) <- model_
    train.error = cbind(train.error, model.train.error)
    colnames(model.test.error) <- model_
    test.error = cbind(test.error, model.test.error)
  }
  avg.train.error = cbind(avg.train.error, rowMeans(train.error))
  avg.test.error = cbind(avg.test.error, rowMeans(test.error))
}


train.test.error = data.frame(train = head(rowMeans(avg.train.error), n=-1), test = head(rowMeans(avg.test.error), n=-1))

dev.new()
barplot(t(as.matrix(train.test.error[order(train.test.error$test, decreasing=TRUE), ])), 
        legend = colnames(train.test.error),
        ylab = 'MSE ($)',
        beside = TRUE)

mem.coefs3 = coef(fit.mem3)$model
depr.estimates = data.frame(model = rownames(mem.coefs3), 
                            depr.year = 100 * (1 - 10^(mem.coefs3$Edad)), 
                            row.names = 1)

################################################################
#
# Plots
#
################################################################

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
nPlot = 0
sortedModels = c("Pathfinder", "CR-V", "Montero", "X-Trail", "4Runner", 
                 "GrandNomade", "LandCruiser", "GrandVitara")
for (model_ in sortedModels) {
  model.data = subset(cleanCarData, model == model_)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints))
  
  new.bs.cols = data.frame(bs(new.data$Edad, knots = age.knots, Boundary.knots = age.bndy))
  names(new.bs.cols) = c('bs.1', 'bs.2', 'bs.3', 'bs.4', 'bs.5')
  
  new.ns.cols = data.frame(ns(new.data$Edad, knots = age.knots, Boundary.knots = age.bndy))
  names(new.ns.cols) = c('ns.1', 'ns.2', 'ns.3')
  
  new.data = cbind(new.data, new.bs.cols)
  new.data = cbind(new.data, new.ns.cols)

  scatterCol <- rgb(0, 0, 255, max = 255, alpha = 125)
  plot(model.data$Edad, model.data$Precio.USD, 
       xlab='', 
       ylab='', 
       axes = FALSE,
       xlim = c(0, quantile(cleanCarData$Edad, .95)),
       ylim = c((min(cleanCarData$Precio.USD)), (max(cleanCarData$Precio.USD))),
       main = model_,
       col = 'grey')
  if (nPlot %% 2 == 0) {
    axis(side = 2, labels = TRUE, tick = TRUE)
    axis(side = 1, labels = FALSE, tick = FALSE)
  }
  if (nPlot == length(availableModels) - 2 | nPlot == length(availableModels) - 1) {
    axis(side = 1, labels = TRUE, tick = TRUE)
    axis(side = 2, labels = FALSE, tick = FALSE)
  }
  box(which = "plot", lty = "solid")
  lines(new.data$Edad, 10^predict(fit.fe[[model_]], new.data), col = 'blue', type = 'o', pch=15)
  lines(new.data$Edad, 10^predict(fit.mem3, new.data), col = 'blue', type = 'o', pch=19)
  lines(new.data$Edad, 10^predict(fit.spline.fe[[model_]], new.data), col = 'red', type = 'o', pch=15)
  lines(new.data$Edad, 10^predict(fit.mem4, new.data), col = 'red', type = 'o', pch=19)
  lines(new.data$Edad, 10^predict(fit.mem4.ind, new.data), col = 'red', type = 'o', pch=25)
  lines(new.data$Edad, 10^predict(fit.ns.fe[[model_]], new.data), col = 'brown', type = 'o', pch=15)
  lines(new.data$Edad, 10^predict(fit.mem.ns, new.data), col = 'brown', type = 'o', pch=19)
  lines(new.data$Edad, 10^predict(fit.mem.ns.ind, new.data), col = 'brown', type = 'o', pch=25)
  lines(new.data$Edad, 10^predict(fit.gams[[model_]], new.data), col = 'pink', type = 'o', pch=15)
  
  if (nPlot == 1) {
    legend("topright", 
           legend = model.names,
           col = c("blue", "blue", "red", "red", "red", "brown", "brown", "brown", "pink"),
           pch = c(15, 19, 15, 19, 25, 15, 19, 25, 15),
           lty = c(1, 1, 1))
  }
  nPlot = nPlot + 1
}

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
nPlot = 0
for (model_ in sortedModels) {
  model.data = subset(cleanCarData, model == model_)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints))
  
  diff.prediction <- c(NA, diff(predict(fit.mem4, new.data)))
  pct.depreciation <- 100 * (1 - 10^(diff.prediction))
  plot(new.data$Edad, 
       pct.depreciation, 
       col = 'red', 
       type = 'o',
       pch = 19,
       xlab='', 
       ylab='', 
       axes = FALSE,
       xlim = c(1, quantile(cleanCarData$Edad, .99)),
       ylim = c(-5, 25),
       main = model_)
  if (nPlot %% 2 == 0) {
    axis(side = 2, labels = TRUE, tick = TRUE)
    axis(side = 1, labels = FALSE, tick = FALSE)
  }
  if (nPlot == length(availableModels) - 2 | nPlot == length(availableModels) - 1) {
    axis(side = 1, labels = TRUE, tick = TRUE)
    axis(side = 2, labels = FALSE, tick = FALSE)
  }
  box(which = "plot", lty = "solid")
  
  loglin.prediction <- predict(fit.fe[[model_]], new.data)
  loglin.diff.prediction <- c(NA, diff(loglin.prediction))
  loglin.pct.prediction <- 100 * (1 - 10^(loglin.diff.prediction))
  lines(new.data$Edad, loglin.pct.prediction, col = 'blue', type = 'o', pch = 15)
  
  lines(new.data$Edad, rep(depr.estimates[model_, ], nPoints), col = 'blue', type = 'o', pch = 19)
  
  spline.loglin.prediction <- predict(fit.spline.fe[[model_]], new.data)
  spline.loglin.diff.prediction <- c(NA, diff(spline.loglin.prediction))
  spline.loglin.pct.prediction <- 100 * (1 - 10^(spline.loglin.diff.prediction))
  lines(new.data$Edad, spline.loglin.pct.prediction, col = 'red', type = 'o', pch = 15)
  
  ns.fe.prediction <- predict(fit.ns.fe[[model_]], new.data)
  ns.fe.diff.prediction <- c(NA, diff(ns.fe.prediction))
  ns.fe.pct.depreciation <- 100 * (1 - 10^(ns.fe.diff.prediction))
  lines(new.data$Edad, ns.fe.pct.depreciation, col = 'brown', type = 'o')
  
  ns.mem.prediction <- predict(fit.mem.ns, new.data)
  ns.mem.diff.prediction <- c(NA, diff(ns.mem.prediction))
  ns.mem.pct.depreciation <- 100 * (1 - 10^(ns.mem.diff.prediction))
  lines(new.data$Edad, ns.mem.pct.depreciation, col = 'brown', type = 'o', pch = 19)
  
  gam.prediction <- predict(fit.gams[[model_]], new.data)
  gam.diff.prediction <- c(NA, diff(gam.prediction))
  gam.pct.depreciation <- 100 * (1 - 10^(gam.diff.prediction))
  lines(new.data$Edad, gam.pct.depreciation, col = 'pink', type = 'o', pch=15)
  
  lines(new.data$Edad, rep(0, length(new.data$Edad)), type='l', col='black')
  if (nPlot == 1) {
    legend("topright", 
           legend = model.names,
           col = c("blue", "blue", "red", "red", "brown", "brown", "pink"),
           pch = c(15, 19, 15, 19, 15, 19, 15),
           lty = c(1, 1, 1),
           ncol = 2)
  }
  nPlot = nPlot + 1
}

#plot(fit.mem3, type = c("p", "smooth"))

#qqmath(fit.mem3, id = 0.5)

#plot(fit.mem4, type = c("p", "smooth"))

#qqmath(fit.mem4, id = 0.5)


