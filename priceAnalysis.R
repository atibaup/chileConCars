# Copyright 2016 - Arnau Tibau-Puig
# This program is distributed under a GNU General Public License 
source("scrapingLib.R")
require(ggplot2)
require(gridExtra)
require(lme4)

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

plt <- ggplot(cleanCarData, aes(x = Edad, y = Kilómetros)) + 
  geom_point() + 
  geom_smooth(method = "lm") 
print(plt)

plt <- ggplot(cleanCarData, aes(x = Kilómetros.miles, y = log10(Precio.USD))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ model)
print(plt)

# price  ~ Age | model
fit.mem = lmer(log10(Precio.USD) ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.mem))
mem.coefs = coef(fit.mem)$model
print("Depreciation (% / year)")
print(cbind(rownames(mem.coefs), 100 * (1 - 10^(mem.coefs$Edad))))

dev.new()
model.plots <- list()
for (model_ in availableModels) {
  model.slope <- mem.coefs[model_, ]$Edad
  model.intercept <- mem.coefs[model_, "(Intercept)"]
  model.data = subset(cleanCarData, model == model_)
  plot <- ggplot(model.data, aes(x = Edad, y = log10(Precio.USD))) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    geom_abline(slope = model.slope, intercept = model.intercept) +
    ggtitle(model_)
  model.plots <- c(model.plots, list(plot))
}
marrangeGrob(grobs = as.list(model.plots), 
             ncol = 2, 
             nrow = ceiling(length(availableModels) / 2))

#
# price ~ mileage
#

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
marrangeGrob(grobs = as.list(model.plots), 
             ncol = 2, 
             nrow = ceiling(length(availableModels) / 2))

#
# price ~ mileage and edad
#

# price  ~ Age
fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + Edad + Kilómetros.miles +  (1 + Edad | model), cleanCarData)
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

#
# price ~ mileage and edad
#

# Kilómetros  ~ Age | model
fit.km.vs.age = lmer(Kilómetros.miles ~ 1 + Edad + (1 + Edad | model), cleanCarData)
print(summary(fit.km.vs.age))
save(fit.km.vs.age, file = "data/fittedKmVsAge.RData")


#
# Fair price predictor
#
getFairPrice <- function(model, year, km) {
  age = getCurrentYear() - year
  model.coefs = mem.coefs3[model, ]
  log.estimate =  model.coefs$`(Intercept)` + model.coefs$Edad * age + model.coefs$Kilómetros.miles * (km / 1000)
  return(10^log.estimate)
}

whatCanIAfford <- function(budget, pc = 10) {
  budgetMax <- budget * (1 + pc / 100)
  budgetMin <- budget * (1 - pc / 100)
  results <- data.frame(stringsAsFactors = FALSE)
  for (model in rownames(mem.coefs3)) {
    ageMax <- (log10(budgetMax) - mem.coefs3[model, "(Intercept)"]) / mem.coefs3[model, "Edad"]
    ageMin <- (log10(budgetMin) - mem.coefs3[model, "(Intercept)"]) / mem.coefs3[model, "Edad"]
    results <- rbind(results, data.frame(model = model, yearMin = 2016 - ageMax, yearMax = 2016 - ageMin, stringsAsFactors = FALSE))
  }
  return(results)
}

