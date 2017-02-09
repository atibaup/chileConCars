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

# Show sample size per model
print(sort(round(summary(cleanCarData$model)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

# Show sample size per transmission
print(sort(round(summary(cleanCarData$Transmisión)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

# Show sample size per combustible
print(sort(round(summary(cleanCarData$Combustible)/nrow(cleanCarData) *  100, 1), decreasing = TRUE))

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
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1, size = textSize),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = textSize),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = textSize, margin = margin(.01, 0, .01, 0, "cm")),
        panel.spacing = unit(0.1, "lines"))

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
        panel.spacing = unit(0.1, "lines"))

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
        panel.spacing = unit(0.1, "lines"))

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

model.rows = c("Precio.USD", "Transmisión", "Combustible", "Edad")
  
trainIndx = createDataPartition(cleanCarData$model, p = 0.8, list = FALSE)
trainData = cleanCarData[trainIndx, ]
testData = cleanCarData[-trainIndx, ]

trainData = trainData[complete.cases(trainData[, model.rows]), ]
testData = testData[complete.cases(testData[, model.rows]), ]

# train global models
fit.mem3 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + Edad + (1 + Edad | model), 
                trainData)
save(fit.mem3, file = "../data/fittedLmer.RData")

age.knots = quantile(cleanCarData$Edad, probs = c(1/3, 2/3))
age.bndy = c(0, max(cleanCarData$Edad))

fit.mem4 = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible + bs(Edad, knots = age.knots, Boundary.knots = age.bndy) + 
                  (1 + bs(Edad, knots = age.knots, Boundary.knots = age.bndy) | model), 
                data=trainData)

save(fit.mem4, file = "../data/fittedSmoothLmer.RData")

fit.mem.ns = lmer(log10(Precio.USD) ~ 1 + Transmisión + Combustible +
                    ns(Edad, knots = age.knots, Boundary.knots = age.bndy) + 
                    (1 + ns(Edad, knots = age.knots, Boundary.knots = age.bndy) | model), 
                  data=trainData)

# train model-wise models
fit.fe = list()
for (model_ in availableModels) {
  model.train.data = subset(trainData, model == model_)
  
  fit.fe[[model_]] = lm(log10(Precio.USD) ~ 1 + Transmisión + Edad, data = model.train.data)
}

mem.coefs3 = coef(fit.mem3)$model
depr.estimates = data.frame(model = rownames(mem.coefs3), 
                            depr.year = 100 * (1 - 10^(mem.coefs3$Edad)), 
                            row.names = 1)

print("Depreciation (% / year) Depreciation (% / 10.000km)")
print(depr.estimates)

dev.new()
nRows = ceiling(length(availableModels)/2)
par(mfrow = c(nRows, 2),
    oma = c(2,2,0.5,0.5) + 0.1,
    mar = c(0,0,2,1) + 0.1)
nPlot = 0
sortedModels = rownames(depr.estimates)[order(depr.estimates$depr.year, decreasing = TRUE)]
for (model_ in sortedModels) {
  model.slope <- mem.coefs3[model_, ]$Edad
  model.intercept <- mem.coefs3[model_, "(Intercept)"]
  
  model.data = subset(cleanCarData, model == model_)

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
       xlim = c(0, quantile(cleanCarData$Edad, .99)),
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

