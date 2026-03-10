# 3/10/26
# Kurt Riggin
# Fitting models to binned Anthropogenic edge data
library(tidyverse)
library(ggpubr)
library(segmented)
library(chngpt)
library(minpack.lm)
library(rcompanion)



anthBinData <- read.csv("anthBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgAnthDist = seq(min(anthBinData$wtAvgAnthDist, na.rm = T),
                      max(anthBinData$wtAvgAnthDist, na.rm = T),
                      length.out = 200)
)



########################################################################################
# % Time Resting
anthBinDataRestSub <- anthBinData %>% 
  filter(!is.na(wtAvgRestPct))
########################################################################################

# null
nullRestPct <- lm(data = anthBinDataRestSub, formula = wtAvgRestPct ~ 1, weights = nMonkeys)
nullRestPctAIC <- AIC(nullRestPct)
predData$nullRestPct <- predict(nullRestPct, newdata = predData)

nullRestPctplot <- ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Null Model (AIC = ", round(nullRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullRestPct))

# linear
linearRestPct <- lm(data = anthBinDataRestSub, formula = wtAvgRestPct ~ wtAvgAnthDist, weights = nMonkeys)
linearRestPctAIC <- AIC(linearRestPct)
predData$linearRestPct <- predict(linearRestPct, newdata = predData)

linearRestPctplot <-ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Linear Model (AIC = ", round(linearRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearRestPct))


# power 
powerabcRestPct <- nlsLM(wtAvgRestPct ~ a * ((wtAvgAnthDist)^b) + c, data = anthBinDataRestSub, 
                         start = list(a = 1, b = 1, c = 80), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcRestPctAIC <- AIC(powerabcRestPct)
predData$powerabcRestPct <- predict(powerabcRestPct, newdata = predData)

powerabcRestPctplot <-ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Power Model (AIC = ", round(powerabcRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcRestPct))




# logistic
logisticRestPct <- nlsLM(wtAvgRestPct ~ a/(1+(b * exp(-c*(wtAvgAnthDist-100)/400))) + d, data = anthBinDataRestSub, 
                         start = list(a = 4, b = 240, c = 105, d = 80), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticRestPctAIC <- AIC(logisticRestPct)
predData$logisticRestPct <- predict(logisticRestPct, newdata = predData)

logisticRestPctplot <-ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Logistic Model (AIC = ", round(logisticRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticRestPct))


# segmented
segmentedRestPct <- segmented(linearRestPct, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedRestPctAIC<- AIC(segmentedRestPct)
predData$segmentedRestPct <- predict(segmentedRestPct, newdata = predData)

segmentedRestPctplot <-ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Segmented Model (AIC = ", round(segmentedRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedRestPct))



# stepwise
stepwiseRestPct <- chngptm(
  formula.1 = wtAvgRestPct ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataRestSub
)
stepwiseRestPctAIC <- AIC(stepwiseRestPct)
predData$stepwiseRestPct <- predict(stepwiseRestPct, newdata = predData)

stepwiseRestPctplot <-ggplot(anthBinDataRestSub, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseRestPct))


# plot title
plottitleRestPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Percent Time Resting", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsRestPct <- ggarrange(nullRestPctplot, 
                             linearRestPctplot, 
                             powerabcRestPctplot, 
                             logisticRestPctplot, 
                             segmentedRestPctplot, 
                             stepwiseRestPctplot,
                             plottitleRestPct, ncol = 3, nrow = 3)

# ggexport(allPlotsRestPct, filename = "anthRestPctModels.pdf", height = 15, width = 15)

########################################################################################
# % Time Moving
anthBinDataMovingSub <- anthBinData %>% 
  filter(!is.na(wtAvgMovingPct))
########################################################################################

# null
nullMovingPct <- lm(data = anthBinDataMovingSub, formula = wtAvgMovingPct ~ 1, weights = nMonkeys)
nullMovingPctAIC <- AIC(nullMovingPct)
predData$nullMovingPct <- predict(nullMovingPct, newdata = predData)

nullMovingPctplot <- ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Null Model (AIC = ", round(nullMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullMovingPct))

# linear
linearMovingPct <- lm(data = anthBinDataMovingSub, formula = wtAvgMovingPct ~ wtAvgAnthDist, weights = nMonkeys)
linearMovingPctAIC <- AIC(linearMovingPct)
predData$linearMovingPct <- predict(linearMovingPct, newdata = predData)

linearMovingPctplot <-ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Linear Model (AIC = ", round(linearMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearMovingPct))


# power 
powerabcMovingPct <- nlsLM(wtAvgMovingPct ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataMovingSub, 
                           start = list(a = 0, b = 1, c = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcMovingPctAIC <- AIC(powerabcMovingPct)
predData$powerabcMovingPct <- predict(powerabcMovingPct, newdata = predData)

powerabcMovingPctplot <-ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Power Model (AIC = ", round(powerabcMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcMovingPct))






# logistic
logisticMovingPct <- nlsLM(wtAvgMovingPct ~ a/(1+(b * exp(-c*(wtAvgAnthDist)/400))) + d, data = anthBinDataMovingSub, 
                           start = list(a = 10, b = 10, c = 10, d = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticMovingPctAIC <- AIC(logisticMovingPct)
predData$logisticMovingPct <- predict(logisticMovingPct, newdata = predData)

logisticMovingPctplot <-ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Logistic Model (AIC = ", round(logisticMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticMovingPct))


# segmented
segmentedMovingPct <- segmented(linearMovingPct, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedMovingPctAIC<- AIC(segmentedMovingPct)
predData$segmentedMovingPct <- predict(segmentedMovingPct, newdata = predData)

segmentedMovingPctplot <-ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Segmented Model (AIC = ", round(segmentedMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedMovingPct))



# stepwise
stepwiseMovingPct <- chngptm(
  formula.1 = wtAvgMovingPct ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataMovingSub
)
stepwiseMovingPctAIC <- AIC(stepwiseMovingPct)
predData$stepwiseMovingPct <- predict(stepwiseMovingPct, newdata = predData)

stepwiseMovingPctplot <-ggplot(anthBinDataMovingSub, aes(x = wtAvgAnthDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseMovingPct))



# plot title
plottitleMovingPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Percent Time Moving", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsMovingPct <- ggarrange(nullMovingPctplot, 
                               linearMovingPctplot, 
                               powerabcMovingPctplot, 
                               logisticMovingPctplot, 
                               segmentedMovingPctplot, 
                               stepwiseMovingPctplot,
                               plottitleMovingPct, ncol = 3, nrow = 3)


# ggexport(allPlotsMovingPct, filename = "anthMovingPctModels.pdf", height = 15, width = 15)


########################################################################################
# % Time Feeding
anthBinDataFeedingSub <- anthBinData %>% 
  filter(!is.na(wtAvgFeedingPct))
########################################################################################

# null
nullFeedingPct <- lm(data = anthBinDataFeedingSub, formula = wtAvgFeedingPct ~ 1, weights = nMonkeys)
nullFeedingPctAIC <- AIC(nullFeedingPct)
predData$nullFeedingPct <- predict(nullFeedingPct, newdata = predData)

nullFeedingPctplot <- ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Null Model (AIC = ", round(nullFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullFeedingPct))

# linear
linearFeedingPct <- lm(data = anthBinDataFeedingSub, formula = wtAvgFeedingPct ~ wtAvgAnthDist, weights = nMonkeys)
linearFeedingPctAIC <- AIC(linearFeedingPct)
predData$linearFeedingPct <- predict(linearFeedingPct, newdata = predData)

linearFeedingPctplot <-ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Linear Model (AIC = ", round(linearFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearFeedingPct))


feedingPR2 <- nagelkerke(linearFeedingPct)
feedingPR2 <- feedingPR2$Pseudo.R.squared.for.model.vs.null


# power 
powerabcFeedingPct <- nlsLM(wtAvgFeedingPct ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataFeedingSub, 
                            start = list(a = 0, b = 1, c = 10), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcFeedingPctAIC <- AIC(powerabcFeedingPct)
predData$powerabcFeedingPct <- predict(powerabcFeedingPct, newdata = predData)

powerabcFeedingPctplot <-ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Power Model (AIC = ", round(powerabcFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcFeedingPct))




# logistic
logisticFeedingPct <- nlsLM(wtAvgFeedingPct ~ a/(1+(b * exp(-c*(wtAvgAnthDist)/400))) + d, data = anthBinDataFeedingSub, 
                            start = list(a = 10, b = 10, c = 10, d = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticFeedingPctAIC <- AIC(logisticFeedingPct)
predData$logisticFeedingPct <- predict(logisticFeedingPct, newdata = predData)

logisticFeedingPctplot <-ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Logistic Model (AIC = ", round(logisticFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticFeedingPct))


# segmented
segmentedFeedingPct <- segmented(linearFeedingPct, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedFeedingPctAIC<- AIC(segmentedFeedingPct)
predData$segmentedFeedingPct <- predict(segmentedFeedingPct, newdata = predData)

segmentedFeedingPctplot <-ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Segmented Model (AIC = ", round(segmentedFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedFeedingPct))



# stepwise
stepwiseFeedingPct <- chngptm(
  formula.1 = wtAvgFeedingPct ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataFeedingSub
)
stepwiseFeedingPctAIC <- AIC(stepwiseFeedingPct)
predData$stepwiseFeedingPct <- predict(stepwiseFeedingPct, newdata = predData)

stepwiseFeedingPctplot <-ggplot(anthBinDataFeedingSub, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseFeedingPct))




# plot title
plottitleFeedingPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Percent Time Feeding", 
           hjust = 0.5, vjust = 0, size = 5)

allPlotsFeedingPct <- ggarrange(nullFeedingPctplot, 
                                linearFeedingPctplot, 
                                powerabcFeedingPctplot, 
                                logisticFeedingPctplot, 
                                segmentedFeedingPctplot, 
                                stepwiseFeedingPctplot,
                                plottitleFeedingPct, ncol = 3, nrow = 3)


# ggexport(allPlotsFeedingPct, filename = "anthFeedingPctModels.pdf", height = 15, width = 15)



########################################################################################
# NumNN
anthBinDataNumNNSub <- anthBinData %>% 
  filter(!is.na(wtAvgNumNN))
########################################################################################

# null
nullNumNN <- lm(data = anthBinDataNumNNSub, formula = wtAvgNumNN ~ 1, weights = nMonkeys)
nullNumNNAIC <- AIC(nullNumNN)
predData$nullNumNN <- predict(nullNumNN, newdata = predData)

nullNumNNplot <- ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullNumNN))

# linear
linearNumNN <- lm(data = anthBinDataNumNNSub, formula = wtAvgNumNN ~ wtAvgAnthDist, weights = nMonkeys)
linearNumNNAIC <- AIC(linearNumNN)
predData$linearNumNN <- predict(linearNumNN, newdata = predData)

numnnPR2 <- nagelkerke(linearNumNN)
numnnPR2 <- numnnPR2$Pseudo.R.squared.for.model.vs.null


linearNumNNplot <-ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearNumNN))


# power 
powerabcNumNN <- nlsLM(wtAvgNumNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataNumNNSub, 
                       start = list(a = 0, b = 1, c = 1), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcNumNNAIC <- AIC(powerabcNumNN)
predData$powerabcNumNN <- predict(powerabcNumNN, newdata = predData)

powerabcNumNNplot <-ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Power Model (AIC = ", round(powerabcNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcNumNN))






# logistic
logisticNumNN <- nlsLM(wtAvgNumNN ~ a/(1+(b * exp(-c*(wtAvgAnthDist)/400))) + d, data = anthBinDataNumNNSub, 
                       start = list(a = 10, b = 10, c = 10, d = 1), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticNumNNAIC <- AIC(logisticNumNN)
predData$logisticNumNN <- predict(logisticNumNN, newdata = predData)

logisticNumNNplot <-ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticNumNN))


# segmented
segmentedNumNN <- segmented(linearNumNN, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedNumNNAIC<- AIC(segmentedNumNN)
predData$segmentedNumNN <- predict(segmentedNumNN, newdata = predData)

segmentedNumNNplot <-ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNumNN))



# stepwise
stepwiseNumNN <- chngptm(
  formula.1 = wtAvgNumNN ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataNumNNSub
)
stepwiseNumNNAIC <- AIC(stepwiseNumNN)
predData$stepwiseNumNN <- predict(stepwiseNumNN, newdata = predData)

stepwiseNumNNplot <-ggplot(anthBinDataNumNNSub, aes(x = wtAvgAnthDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseNumNN))





# plot title
plottitleNumNN <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic # of Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNumNN <- ggarrange(nullNumNNplot, 
                           linearNumNNplot, 
                           powerabcNumNNplot, 
                           logisticNumNNplot, 
                           segmentedNumNNplot, 
                           stepwiseNumNNplot,
                           plottitleNumNN, ncol = 3, nrow = 3)


# ggexport(allPlotsNumNN, filename = "anthNumNNModels.pdf", height = 15, width = 15)


########################################################################################
# DistNN
anthBinDataDistNNSub <- anthBinData %>% 
  filter(!is.na(wtAvgDistNN))
########################################################################################

# null
nullDistNN <- lm(data = anthBinDataDistNNSub, formula = wtAvgDistNN ~ 1, weights = nMonkeys)
nullDistNNAIC <- AIC(nullDistNN)
predData$nullDistNN <- predict(nullDistNN, newdata = predData)

nullDistNNplot <- ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullDistNN))

# linear
linearDistNN <- lm(data = anthBinDataDistNNSub, formula = wtAvgDistNN ~ wtAvgAnthDist, weights = nMonkeys)
linearDistNNAIC <- AIC(linearDistNN)
predData$linearDistNN <- predict(linearDistNN, newdata = predData)

linearDistNNplot <-ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearDistNN))


# power 
powerabcDistNN <- nlsLM(wtAvgDistNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataDistNNSub, 
                        start = list(a = 0, b = 1, c = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcDistNNAIC <- AIC(powerabcDistNN)
predData$powerabcDistNN <- predict(powerabcDistNN, newdata = predData)

powerabcDistNNplot <-ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Power Model (AIC = ", round(powerabcDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcDistNN))


distnnPR2 <- nagelkerke(powerabcDistNN, null = nullDistNN)
distnnPR2 <- distnnPR2$Pseudo.R.squared.for.model.vs.null



# logistic
logisticDistNN <- nlsLM(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgAnthDist)/100))) + d, data = anthBinDataDistNNSub, 
                        start = list(a = 1.5, b = 130, c = 4.5, d = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticDistNNAIC <- AIC(logisticDistNN)
predData$logisticDistNN <- predict(logisticDistNN, newdata = predData)

logisticDistNNplot <-ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticDistNN))


# segmented
segmentedDistNN <- segmented(linearDistNN, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedDistNNAIC<- AIC(segmentedDistNN)
predData$segmentedDistNN <- predict(segmentedDistNN, newdata = predData)

segmentedDistNNplot <-ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedDistNN))



# stepwise
stepwiseDistNN <- chngptm(
  formula.1 = wtAvgDistNN ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataDistNNSub
)
stepwiseDistNNAIC <- AIC(stepwiseDistNN)
predData$stepwiseDistNN <- predict(stepwiseDistNN, newdata = predData)

stepwiseDistNNplot <-ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseDistNN))



# plot title
plottitleDistNN <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Distance from Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsDistNN <- ggarrange(nullDistNNplot, 
                            linearDistNNplot, 
                            powerabcDistNNplot, 
                            logisticDistNNplot, 
                            segmentedDistNNplot, 
                            stepwiseDistNNplot,
                            plottitleDistNN, ncol = 3, nrow = 3)


# ggexport(allPlotsDistNN, filename = "anthDistNNModels.pdf", height = 15, width = 15)

