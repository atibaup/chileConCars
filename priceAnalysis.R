# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 
source("scrapingLib.R")
require(ggplot2)
require(gridExtra)
require(lme4)
require(splines)
library(merTools)
require(gamm4)

yapoData <- read.csv("data/yapoData.csv")
yapoData$X <- NULL
yapoData$source <- "Yapo"

chileautosData <- read.csv("data/chileautosData.csv")
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

availableModels = unique(cleanCarData$model)

# Show sample size per model
print(sort(summary(cleanCarData$model), decreasing = TRUE))

cleanCarData$Kilómetros.miles = cleanCarData$Kilómetros / 1000

dev.new()
plt1 <- ggplot(cleanCarData, aes(x = Kilómetros, col = source)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(cleanCarData, aes(x = Edad, col = source)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt3 <- ggplot(cleanCarData, aes(x = Precio.USD, col = source)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() + 
  facet_wrap(~ model, nrow = length(availableModels), ncol = 1) + 
  theme(legend.position='none', 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pltAll <- marrangeGrob(list(plt1, plt2, plt3), ncol = 3, nrow = 1)
print(pltAll)

plt <- ggplot(cleanCarData, aes(x = Kilómetros.miles, y = log10(Precio.USD))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ model)
print(plt)

#
# price ~ mileage and edad
#

# Kilómetros  ~ Age | model
fit.km.vs.age = lmer(Kilómetros.miles ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.km.vs.age))
save(fit.km.vs.age, file = "data/fittedKmVsAge.RData")


# price  ~ Age | model
fit.mem = lmer(log10(Precio.USD) ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.mem))
mem.coefs = coef(fit.mem)$model
print("Depreciation (% / year)")
print(cbind(rownames(mem.coefs), 100 * (1 - 10^(mem.coefs$Edad))))

# model.plots <- list()
# for (model_ in availableModels) {
#   model.slope <- mem.coefs[model_, ]$Edad
#   model.intercept <- mem.coefs[model_, "(Intercept)"]
#   model.data = subset(cleanCarData, model == model_)
#   plot <- ggplot(model.data, aes(x = Edad, y = log10(Precio.USD))) + 
#     geom_point() + 
#     geom_smooth(method = "lm") + 
#     geom_abline(slope = model.slope, intercept = model.intercept) +
#     ggtitle(model_)
#   model.plots <- c(model.plots, list(plot))
# }
# marrangeGrob(grobs = as.list(model.plots), 
#              ncol = 2, 
#              nrow = ceiling(length(availableModels) / 2))

#
# price ~ mileage
#

fit.mem2 = lmer(log10(Precio.USD) ~  1 + Kilómetros.miles + (1 | model), cleanCarData)
print(summary(fit.mem2))
mem.coefs2 = coef(fit.mem2)$model
print("Depreciation (% / 10.000km)")
print(cbind(rownames(mem.coefs2), 100 * (1 - 10^(mem.coefs2$Kilómetros.miles))))

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

# price  ~ Age
fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible +
                  Edad + Kilómetros.miles +  (1 + Edad | model), cleanCarData)
print(summary(fit.mem3))
mem.coefs3 = coef(fit.mem3)$model
depr.estimates = data.frame(model = rownames(mem.coefs3), 
                            depr.year = 100 * (1 - 10^(mem.coefs3$Edad)), 
                            depr.dist = 100 * (1 - 10^(mem.coefs3$Kilómetros.miles)),
                            row.names = 1)

print("Depreciation (% / year) Depreciation (% / 10.000km)")
print(depr.estimates)
save(fit.mem3, file = "data/fittedLmer.RData")

model.plots <- list()
for (model_ in availableModels) {
  model.slope <- mem.coefs3[model_, ]$Edad
  model.intercept <- mem.coefs3[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  model.plot <- ggplot(model.data, aes(x = Edad, y = log10(Precio.USD))) + 
    ylim(3.4, 4.8) +
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_smooth(method = "loess") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(sprintf("%s %.2f (%%/year)", model_, 
                    depr.estimates[model_, ]$depr.year))
  model.plots <- c(model.plots, list(model.plot))
}

plt <- marrangeGrob(grobs = as.list(model.plots), 
             ncol = 2, 
             nrow = ceiling(length(availableModels) / 2))
print(plt)

shadeBetween <- function(x, y1, y2, ...) {
  #plot(x, y1, col = 'black', type='l', ylim = c(min(c(y1, y2)), max(c(y1, y2))))
  #lines(x, y2, col = 'black', type='l')
  polygon(c(x, rev(x)), c(y2, rev(y1)), ...)
}

bootstrapped.CI <- function(fit, new.data, p = 0.10, nSim = 1000) {
  sim <- bootMer(fit, 
                 function(x) predict(x, newdata = new.data), 
                 nsim = nSim, 
                 .progress = "txt")
  u.ci <- apply(sim$t, 2, function(x) quantile(x, p, na.rm = TRUE))
  l.ci <- apply(sim$t, 2, function(x) quantile(x, 1-p, na.rm = TRUE))
  return(cbind(new.data, data.frame(upr = u.ci, lwr = l.ci)))
}

makeTransparent <- function(color, alpha) {
  col = col2rgb("green")
  rgb(col[1], col[2], col[3], alpha = alpha * 255, maxColorValue = 255)
}

fit.mem4 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible +
                  bs(Edad, df = 5) + Kilómetros.miles +  
                  (1 + bs(Edad, df = 5) | model), 
                data=cleanCarData)

save(fit.mem4, file = "data/fittedSmoothLmer.RData")

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
nPlot = 0
sortedModels = rownames(depr.estimates)[order(depr.estimates$depr.year, decreasing = TRUE)]
fit.gams.predictions = list()
for (model_ in sortedModels) {
  model.data = subset(cleanCarData, model == model_)
  fit.gams = gam(log10(Precio.USD) ~ 1 + Transmisión +
                    s(Edad) + Kilómetros.miles, 
                  data=model.data)
  fit.gams.predictions[[nPlot + 1]] = predict(fit.gams, new.data)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints),
                        Kilómetros.miles = predict(fit.km.vs.age, 
                                                   newdata = data.frame(Edad = age.points, model = rep(model_, nPoints))))
  scatterCol <- rgb(0, 0, 255, max = 255, alpha = 125)
  title <- sprintf("%s %.2f (%%/year)", model_, depr.estimates[model_, ]$depr.year)
  plot(model.data$Edad, model.data$Precio.USD, 
       xlab='', 
       ylab='', 
       axes = FALSE,
       xlim = c(0, quantile(cleanCarData$Edad, .95)),
       ylim = c((min(cleanCarData$Precio.USD)), (max(cleanCarData$Precio.USD))),
       main = title)
  if (nPlot %% 2 == 0) {
    axis(side = 2, labels = TRUE, tick = TRUE)
    axis(side = 1, labels = FALSE, tick = FALSE)
  }
  if (nPlot == length(availableModels) - 2 | nPlot == length(availableModels) - 1) {
    axis(side = 1, labels = TRUE, tick = TRUE)
    axis(side = 2, labels = FALSE, tick = FALSE)
  }
  box(which = "plot", lty = "solid")
  lines(new.data$Edad, 10^predict(fit.mem3, new.data), col = 'green', type = 'o')
  lines(new.data$Edad, 10^predict(fit.mem4, new.data), col = 'blue', type = 'o')
  lines(new.data$Edad, 10^fit.gams.predictions[[nPlot + 1]], col = 'red', type = 'o')
  if (nPlot == 0) {
    legend("topright", 
           legend = c("Log-linear MEM", "Spline log-linear MEM", "GAM"),
           col = c("green", "blue", "red"),
           lty = c(1, 1, 1))
  }
  # fit.mem3.ci <- predictInterval(fit.mem3, new.data)
  # fit.mem4.ci <- predictInterval(fit.mem4, new.data)
  # 
  # shadeBetween(new.data$Edad, 10^fit.mem3.ci$lwr, 10^fit.mem3.ci$upr, col = makeTransparent("green", 0.1))
  # shadeBetween(new.data$Edad, 10^fit.mem4.ci$lwr, 10^fit.mem4.ci$upr, col = makeTransparent("blue", 0.1))
  # 
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
  fit.gams = gam(log10(Precio.USD) ~ 1 + Transmisión +
                   s(Edad) + Kilómetros.miles, 
                 data=model.data)
  age.points = seq(0, max(cleanCarData$Edad), by = 1)
  nPoints = length(age.points)
  new.data = data.frame(Edad = age.points, 
                        Transmisión = rep("Automático", nPoints),
                        Combustible = rep("Bencina", nPoints),
                        model = rep(model_, nPoints),
                        Kilómetros.miles = predict(fit.km.vs.age, 
                                                   newdata = data.frame(Edad = age.points, model = rep(model_, nPoints))))
  title <- sprintf("%s %.2f (%%/year)", model_, depr.estimates[model_, ]$depr.year)
  diff.prediction <- c(NA, diff(fit.gams.predictions[[nPlot + 1]]))
  pct.depreciation <- 100 * (1 - 10^(diff.prediction))
  plot(new.data$Edad, 
       pct.depreciation, 
       col = 'green', 
       type = 'o',
       xlab='', 
       ylab='', 
       axes = FALSE,
       xlim = c(0, quantile(cleanCarData$Edad, .95)),
       ylim = c(0, 20),
       main = title)
  if (nPlot %% 2 == 0) {
    axis(side = 2, labels = TRUE, tick = TRUE)
    axis(side = 1, labels = FALSE, tick = FALSE)
  }
  if (nPlot == length(availableModels) - 2 | nPlot == length(availableModels) - 1) {
    axis(side = 1, labels = TRUE, tick = TRUE)
    axis(side = 2, labels = FALSE, tick = FALSE)
  }
  box(which = "plot", lty = "solid")
  lines(new.data$Edad, rep(depr.estimates[model_, ]$depr.year, nPoints), col = 'blue', type = 'o')
  gam.prediction <- predict(fit.gams, new.data)
  gam.diff.prediction <- c(NA, diff(gam.prediction))
  gam.pct.depreciation <- 100 * (1 - 10^(gam.diff.prediction))
  lines(new.data$Edad, gam.pct.depreciation, col = 'red', type = 'o')
  if (nPlot == 0) {
    legend("topright", 
           legend = c("Log-linear MEM", "Spline log-linear MEM", "GAM"),
           col = c("green", "blue", "red"),
           lty = c(1, 1, 1))
  }
  nPlot = nPlot + 1
}

#
# Fair price predictor
#
getFairPrice <- function(model, year, km) {
  age = getCurrentYear() - year
  model.coefs = mem.coefs3[model, ]
  log.estimate =  model.coefs$`(Intercept)` + model.coefs$Edad * age + model.coefs$Kilómetros.miles * (km / 1000)
  return(10^log.estimate)
}

whatCanIAfford <- function(budget, fit = fit.mem4, pc = 10, maxAge = 20) {
  budgetMax <- budget * (1 + pc / 100)
  budgetMin <- budget * (1 - pc / 100)
  results <- data.frame(stringsAsFactors = FALSE)
  age.points = seq(0, maxAge, by = 1)
  nPoints = length(age.points)
  currentYear = getCurrentYear()
  for (model in availableModels) {
    predicted.km.miles <- predict(fit.km.vs.age, 
                                  newdata = data.frame(Edad = age.points, model = rep(model, nPoints)))
    new.data <- data.frame(Edad = age.points, 
                          Transmisión = rep("Automático", nPoints),
                          Combustible = rep("Bencina", nPoints),
                          model = rep(model, nPoints),
                          Kilómetros.miles = predicted.km.miles)
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

