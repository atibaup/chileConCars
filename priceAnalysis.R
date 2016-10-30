# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 

require(ggplot2)
require(lme4)

carData <- read.csv("yapoData.csv")

# data cleaning:
# 1) Remove entries with 0 kilómetros
minKm = 10
maxKm = 5e5
cleanCarData = subset(carData, carData$Kilómetros > minKm & carData$Kilómetros < maxKm)

# 2) Remove entries with obviously spurious prices
minPrice <- 4000
maxPrice <- 50000
cleanCarData = subset(cleanCarData, cleanCarData$Precio.USD >= minPrice & cleanCarData$Precio.USD <= maxPrice)

cleanCarData$Kilómetros.miles = cleanCarData$Kilómetros / 1000

dev.new()
ggplot(cleanCarData, aes(x = Kilómetros)) + geom_histogram(aes(y = ..density..)) + geom_density() + facet_wrap(~ model)

dev.new()
ggplot(cleanCarData, aes(x = Edad)) + geom_histogram(aes(y = ..density..)) + geom_density() + facet_wrap(~ model)

dev.new()
ggplot(cleanCarData, aes(x = Precio)) + geom_histogram(aes(y = ..density..)) + geom_density() + facet_wrap(~ model)

dev.new()
ggplot(cleanCarData, aes(x = Edad, y = Kilómetros)) + geom_point() + geom_smooth(method = "lm") 


dev.new()
ggplot(cleanCarData, aes(x = Kilómetros.miles, y = log10(Precio.USD))) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~ model)

fit = lm(log10(Precio.USD) ~ model + Edad -1, cleanCarData)
print(summary(fit))
print("Depreciation (% / year)")
print(100 * (1 - 10^(coef(fit)["Edad"])))

# price  ~ Age
fit.mem = lmer(log10(Precio.USD) ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.mem))
mem.coefs = coef(fit.mem)$model
print("Depreciation (% / year)")
print(cbind(rownames(mem.coefs), 100 * (1 - 10^(mem.coefs$Edad))))

for (model_ in unique(cleanCarData$model)) {
  dev.new()
  model.slope <- mem.coefs[model_, ]$Edad
  model.intercept <- mem.coefs[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  model.plot <- ggplot(model.data, aes(x = Edad, y = log10(Precio.USD))) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(model_)
  print(model.plot)
}

#
# price ~ mileage
#

fit.mem2 = lmer(log10(Precio.USD) ~  1 + Kilómetros.miles + (1 | model), cleanCarData)
print(summary(fit.mem2))
mem.coefs2 = coef(fit.mem2)$model
print("Depreciation (% / 10.000km)")
print(cbind(rownames(mem.coefs2), 100 * (1 - 10^(mem.coefs2$Kilómetros.miles))))

for (model_ in unique(cleanCarData$model)) {
  dev.new()
  model.slope <- mem.coefs2[model_, ]$Kilómetros.miles
  model.intercept <- mem.coefs2[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  model.plot <- ggplot(model.data, aes(x = Kilómetros.miles, y = log10(Precio.USD))) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(model_)
  print(model.plot)
}

#
# price ~ mileage and edad
#

# price  ~ Age
fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Edad + Kilómetros.miles +  (1 + Edad | model), cleanCarData)
print(summary(fit.mem3))
mem.coefs3 = coef(fit.mem3)$model
print("Depreciation (% / year) Depreciation (% / 10.000km)")
print(cbind(rownames(mem.coefs3), 100 * (1 - 10^(mem.coefs3$Edad)), 100 * (1 - 10^(mem.coefs3$Kilómetros.miles))))

for (model_ in unique(cleanCarData$model)) {
  dev.new()
  model.slope <- mem.coefs3[model_, ]$Edad
  model.intercept <- mem.coefs3[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  model.plot <- ggplot(model.data, aes(x = Edad, y = log10(Precio.USD))) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(model_)
  print(model.plot)
}

#
# Fair price predictor
#
getFairPrice <- function(model, year, km) {
  currentYear = as.integer(format(Sys.Date(), "%Y"))
  age = currentYear - year
  model.coefs = mem.coefs3[model, ]
  log.estimate =  model.coefs$`(Intercept)` + model.coefs$Edad * age + model.coefs$Kilómetros.miles * km / 1000
  return(10^log.estimate)
}


