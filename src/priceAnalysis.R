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

yapoData <- read.csv("../data/yapoDataNew.csv")
yapoData$X <- NULL
yapoData$source <- "Yapo"

chileautosData <- read.csv("../data/chileautosData.csv")
chileautosData$X <- NULL
chileautosData$source <- "Chileautos"

carData <- rbind(yapoData, chileautosData)

# data cleaning:
# 1) Remove entries with silly kilómetros
minKm = 10
maxKm = 5e5
cleanCarData = subset(carData, carData$Kilómetros > minKm & carData$Kilómetros < maxKm)

# 2) Remove entries with obviously spurious prices
minPrice <- 4000
maxPrice <- 50000
cleanCarData = subset(cleanCarData, 
                      cleanCarData$Precio.USD >= minPrice & cleanCarData$Precio.USD <= maxPrice)

# 3) Remove entries with bad year data
cleanCarData = subset(cleanCarData, !(model == "4Runner" & Edad == 26 & Precio.USD > 10000))

# 4) Remove entries with Combustible = "Gas" or "Otros" since they add up to 5 samples only
cleanCarData = subset(cleanCarData, !(Combustible %in% c("Gas", "Otros")))

write.csv(cleanCarData, file = "../data/cleanCarData.csv")

availableModels = unique(cleanCarData$model)

# Show sample size per model
print(sort(round(summary(cleanCarData$model)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

# Show sample size per transmission
print(sort(round(summary(cleanCarData$Transmisión)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

# Show sample size per combustible
print(sort(round(summary(cleanCarData$Combustible)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

cleanCarData$Kilómetros.miles = cleanCarData$Kilómetros / 1000


textSize = 4

#
# Data exploration
#

dev.new()

# Sort facet_wrap by model's average Kilómetros
kmByModel = aggregate(Kilómetros ~ model, cleanCarData, mean)
sortedLevels = kmByModel[order(kmByModel$Kilómetros, decreasing = TRUE),]$model
cleanCarData$model <- factor(cleanCarData$model, levels = sortedLevels)

plt1 <- ggplot(cleanCarData, aes(x = Kilómetros)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1, size = textSize),
        axis.title.y=element_blank(),
        axis.title.x = element_text(size = textSize),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size = textSize, margin = margin(.01, 0, .01, 0, "cm")),
        panel.margin = unit(c(0.1, 0, 0), "lines"))

# Sort facet_wrap by model's average Edad
edadByModel = aggregate(Edad ~ model, cleanCarData, mean)
sortedLevels = edadByModel[order(edadByModel$Edad, decreasing = TRUE),]$model
cleanCarData$model <- factor(cleanCarData$model, levels = sortedLevels)

plt2 <- ggplot(cleanCarData, aes(x = Edad)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1, size = textSize),
        axis.title.y=element_blank(),
        axis.title.x = element_text(size = textSize),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size = textSize, margin = margin(.01, 0, .01, 0, "cm")),
        panel.margin = unit(c(0.1,0,0), "lines"))

# Sort facet_wrap by model's average Precio.USD
precioByModel = aggregate(Precio.USD ~ model, cleanCarData, mean)
sortedLevels = precioByModel[order(precioByModel$Precio.USD, decreasing = TRUE),]$model
cleanCarData$model <- factor(cleanCarData$model, levels = sortedLevels)

plt3 <- ggplot(cleanCarData, aes(x = Precio.USD)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1, size = textSize),
        axis.title.x = element_text(size = textSize),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size = textSize, margin = margin(.01, 0, .01, 0, "cm")),
        panel.margin = unit(c(0.1,0,0), "lines"))

pltAll <- marrangeGrob(list(plt1, plt2, plt3), top = "", ncol = 3, nrow = 1)
print(pltAll)
ggsave("/Users/atibaup/atibaup.github.io/assets/summary.png", plot = pltAll, width = 6, height = 6, units = "cm")

#
# price ~ mileage and edad
#

# Kilómetros  ~ Age | model
fit.km.vs.age = lmer(Kilómetros.miles ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.km.vs.age))
save(fit.km.vs.age, file = "../data/fittedKmVsAge.RData")


# price  ~ Age | model
fit.mem = lmer(log10(Precio.USD) ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.mem))
mem.coefs = coef(fit.mem)$model
print("Depreciation (% / year)")
print(cbind(rownames(mem.coefs), 100 * (1 - 10^(mem.coefs$Edad))))

################################################################################################
#
# price ~ mileage
#
################################################################################################

fit.mem2 = lmer(log10(Precio.USD) ~  1 + Kilómetros.miles + (1 | model), cleanCarData)
print(summary(fit.mem2))
mem.coefs2 = coef(fit.mem2)$model
print("Depreciation (% / 10.000km)")
print(cbind(rownames(mem.coefs2), 100 * (1 - 10^(mem.coefs2$Kilómetros.miles))))

dev.new()
model.plots <- list()
for (model_ in availableModels) {
  model.slope <- mem.coefs2[model_, ]$Kilómetros.miles
  model.intercept <- mem.coefs2[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  model.plot <- ggplot(model.data, aes(x = Kilómetros.miles, y = log10(Precio.USD))) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(model_)
  model.plots <- c(model.plots, list(model.plot))
}
plt <- marrangeGrob(grobs = as.list(model.plots), 
             ncol = 2, 
             nrow = ceiling(length(availableModels) / 2))
print(plt)

################################################################################################
#
# price ~ age + other covariates
#
################################################################################################

fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible +
                  Edad + (1 + Edad | model), 
                cleanCarData)
print(summary(fit.mem3))
mem.coefs3 = coef(fit.mem3)$model
depr.estimates = data.frame(model = rownames(mem.coefs3), 
                            depr.year = 100 * (1 - 10^(mem.coefs3$Edad)), 
                            #depr.dist = 100 * (1 - 10^(mem.coefs3$Kilómetros.miles)),
                            row.names = 1)

print("Depreciation (% / year) Depreciation (% / 10.000km)")
print(depr.estimates)
save(fit.mem3, file = "../data/fittedLmer.RData")

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(7,7,0,0) + 0.1,
    mar = c(0,0,2,1) + 0.1)
nPlot = 0
sortedModels = rownames(depr.estimates)[order(depr.estimates$depr.year, decreasing = TRUE)]
fit.fe = list()
for (model_ in sortedModels) {
  model.slope <- mem.coefs3[model_, ]$Edad
  model.intercept <- mem.coefs3[model_, "(Intercept)"]
  
  model.data = subset(cleanCarData, model == model_)
  
  fit.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión + Edad, model.data)
  print(summary(fit.fe[[model_]]))
  model.slope.fe = coef(fit.fe[[model_]])["Edad"]
  model.int.fe = coef(fit.fe[[model_]])["(Intercept)"]
  depr.year.fe = 100 * (1 - 10^model.slope.fe)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints),
                        Kilómetros.miles = predict(fit.km.vs.age, 
                                                   newdata = data.frame(Edad = age.points, model = rep(model_, nPoints))))

  scatterCol <- rgb(0, 0, 255, max = 255, alpha = 125)
  title <- sprintf("%s\n MEM: %.2f %%/yr FE: %.2f %%/yr", 
                   model_, depr.estimates[model_, ], depr.year.fe)
  plot(model.data$Edad, model.data$Precio.USD, 
       axes = FALSE,
       xlim = c(0, quantile(cleanCarData$Edad, .95)),
       ylim = c((min((cleanCarData$Precio.USD))), (max((cleanCarData$Precio.USD)))),
       main = title,
       col = 'grey')
  if (nPlot %% 2 == 0) {
    axis(side = 2, labels = TRUE, tick = TRUE)
    axis(side = 1, labels = FALSE, tick = FALSE, ylab = "Price (USD)")
  }
  if (nPlot == length(availableModels) - 2 | nPlot == length(availableModels) - 1) {
    axis(side = 1, labels = TRUE, tick = TRUE)
    axis(side = 2, labels = FALSE, tick = FALSE, xlab = "Age")
  }
  box(which = "plot", lty = "solid")
  lines(new.data$Edad, 10^predict(fit.fe[[model_]], new.data), col = 'black', type = 'o')
  lines(new.data$Edad, 10^predict(fit.mem3, new.data), col = 'green', type = 'o')
  if (nPlot == 1) {
    legend("topright", 
           legend = c("Log-Linear", "Log-linear MEM"),
           col = c("black", "green"),
           lty = c(1, 1, 1))
  }
  nPlot = nPlot + 1
}

################################################################
#
# Non linear ones
#
################################################################

fit.mem4 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible +
                  bs(Edad, df = 5) + (1 + bs(Edad, df = 5) | model), 
                data=cleanCarData)

save(fit.mem4, file = "../data/fittedSmoothLmer.RData")

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
nPlot = 0
sortedModels = rownames(depr.estimates)[order(depr.estimates$depr.year, decreasing = TRUE)]
fit.gams = list()
fit.spline.fe = list()
for (model_ in sortedModels) {
  model.data = subset(cleanCarData, model == model_)
  fit.gams[[model_]] = gam(log10(Precio.USD) ~ 1 + Transmisión + 
                             s(Edad), 
                           data=model.data)
  fit.spline.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión +
                                   bs(Edad, df = 5), 
                                 data=model.data)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints),
                        Kilómetros.miles = predict(fit.km.vs.age, 
                                                   newdata = data.frame(Edad = age.points, model = rep(model_, nPoints))))

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
  lines(new.data$Edad, 10^predict(fit.fe[[model_]], new.data), col = 'black', type = 'o')
  lines(new.data$Edad, 10^predict(fit.mem3, new.data), col = 'green', type = 'o')
  lines(new.data$Edad, 10^predict(fit.spline.fe[[model_]], new.data), col = 'blue', type = 'o')
  lines(new.data$Edad, 10^predict(fit.mem4, new.data), col = 'red', type = 'o')
  lines(new.data$Edad, 10^predict(fit.gams[[model_]], new.data), col = 'pink', type = 'o')
  if (nPlot == 1) {
    legend("topright", 
           legend = c("Log-Linear", "Log-linear MEM", "Spline log-linear", "Spline log-linear MEM", "GAM"),
           col = c("black", "green", "blue", "red", "pink"),
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
  print(model_)
  model.data = subset(cleanCarData, model == model_)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints),
                        Kilómetros.miles = predict(fit.km.vs.age, 
                                                   newdata = data.frame(Edad = age.points, model = rep(model_, nPoints))))
  diff.prediction <- c(NA, diff(predict(fit.mem4, new.data)))
  pct.depreciation <- 100 * (1 - 10^(diff.prediction))
  plot(new.data$Edad, 
       pct.depreciation, 
       col = 'red', 
       type = 'o',
       xlab='', 
       ylab='', 
       axes = FALSE,
       xlim = c(0, quantile(cleanCarData$Edad, .95)),
       ylim = c(0, 20),
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
  lines(new.data$Edad, loglin.pct.prediction, col = 'black', type = 'o')
  
  lines(new.data$Edad, rep(depr.estimates[model_, ], nPoints), col = 'green', type = 'o')
  
  spline.loglin.prediction <- predict(fit.spline.fe[[model_]], new.data)
  spline.loglin.diff.prediction <- c(NA, diff(spline.loglin.prediction))
  spline.loglin.pct.prediction <- 100 * (1 - 10^(spline.loglin.diff.prediction))
  lines(new.data$Edad, spline.loglin.pct.prediction, col = 'blue', type = 'o')
  
  gam.prediction <- predict(fit.gams[[model_]], new.data)
  gam.diff.prediction <- c(NA, diff(gam.prediction))
  gam.pct.depreciation <- 100 * (1 - 10^(gam.diff.prediction))
  lines(new.data$Edad, gam.pct.depreciation, col = 'pink', type = 'o')
  
  if (nPlot == 1) {
    legend("topright", 
           legend = c("Log-linear", "Log-linear MEM", "Spline log-linear", "Spline log-linear MEM", "GAM"),
           col = c("black", "green", "blue", "red", "pink"),
           lty = c(1, 1, 1))
  }
  nPlot = nPlot + 1
}

dev.new()
plot(fit.mem3, type = c("p", "smooth"))

dev.new()
qqmath(fit.mem3, id = 0.5)

dev.new()
plot(fit.mem4, type = c("p", "smooth"))

dev.new()
qqmath(fit.mem4, id = 0.5)

#
# Fair price predictor
#
getFairPrice <- function(model, year, km, transmision = "Automático", combustible = "Bencina") {
  age = getCurrentYear() - year
  new.data <- data.frame(Edad = age, 
                         Transmisión = transmision,
                         Combustible = combustible,
                         model = model,
                         Kilómetros.miles = km / 1000)
  price.usd = 10^predict(fit.mem4, newdata = new.data)[[1]]
  return(list(usd = price.usd, chp = price.usd / 0.0015))
}

whatCanIAfford <- function(budget, 
                           transmision = "Automático", 
                           combustible = "Bencina", 
                           kilometros.miles = NULL,
                           fit = fit.mem4, 
                           pc = 10, 
                           maxAge = 20) {
  budgetMax <- budget * (1 + pc / 100)
  budgetMin <- budget * (1 - pc / 100)
  results <- data.frame(stringsAsFactors = FALSE)
  age.points = seq(0, maxAge, by = 1)
  nPoints = length(age.points)
  currentYear = getCurrentYear()
  for (model in availableModels) {
    if (is.null(kilometros.miles)) {
      km.miles <- predict(fit.km.vs.age, 
                          newdata = data.frame(Edad = age.points, model = rep(model, nPoints)))
    } else {
      km.miles <- rep(kilometros.miles, nPoints)
    }
    
    new.data <- data.frame(Edad = age.points, 
                          Transmisión = rep(transmision, nPoints),
                          Combustible = rep(combustible, nPoints),
                          model = rep(model, nPoints),
                          Kilómetros.miles = km.miles)
    price.vs.age = 10^predict(fit, newdata = new.data)
    ageMax <- NULL
    ageMin <- NULL
    i <- 1
    while (i <= nPoints & (is.null(ageMax) | is.null(ageMin))) {
      if (price.vs.age[i] <= budgetMax) {
        ageMin <- age.points[i]
      }
      if (price.vs.age[i] < budgetMin & i > 1) {
        ageMax <- age.points[i - 1]
      }
      i =  i + 1
    }
    if (!is.null(ageMin) & !is.null(ageMax)) {
      results <- rbind(results, 
                       data.frame(model = model, 
                                  years = sprintf("%d-%d", currentYear - ageMax, currentYear - ageMin), 
                                  kms = sprintf("%d-%d",
                                                1000 * round(predict(fit.km.vs.age, newdata = data.frame(Edad = ageMax, model = model))),
                                                1000 * round(predict(fit.km.vs.age, newdata = data.frame(Edad = ageMin, model = model)))),
                                  stringsAsFactors = FALSE))
    }
  }
  return(results)
}

