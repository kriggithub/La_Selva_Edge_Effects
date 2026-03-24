# 3/10/26
# Kurt Riggin
# Fitting models to binned River edge data
library(tidyverse)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)
library(rcompanion)



rivBinData <- read.csv("rivBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgRivDist = seq(min(rivBinData$wtAvgRivDist, na.rm = T),
                     max(rivBinData$wtAvgRivDist, na.rm = T),
                     length.out = 200)
)



########################################################################################
# % Time Resting
rivBinDataRestSub <- rivBinData %>% 
  filter(!is.na(wtAvgRestPct))
########################################################################################

# null
nullRestPct <- lm(data = rivBinDataRestSub, formula = wtAvgRestPct ~ 1, weights = nMonkeys)
nullRestPctAIC <- AIC(nullRestPct)
predData$nullRestPct <- predict(nullRestPct, newdata = predData)

nullRestPctplot <- ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Null Model (AIC = ", round(nullRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullRestPct))

# linear
linearRestPct <- lm(data = rivBinDataRestSub, formula = wtAvgRestPct ~ wtAvgRivDist, weights = nMonkeys)
linearRestPctAIC <- AIC(linearRestPct)
predData$linearRestPct <- predict(linearRestPct, newdata = predData)

linearRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Linear Model (AIC = ", round(linearRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearRestPct))


restingPR2 <- nagelkerke(linearRestPct)
restingPR2 <- restingPR2$Pseudo.R.squared.for.model.vs.null


# power 
powerabcRestPct <- nlsLM(wtAvgRestPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataRestSub, 
                         start = list(a = 0, b = 10, c = 80), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcRestPctAIC <- AIC(powerabcRestPct)
predData$powerabcRestPct <- predict(powerabcRestPct, newdata = predData)

powerabcRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Power Model (AIC = ", round(powerabcRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcRestPct))




# logistic
logisticRestPct <- nlsLM(wtAvgRestPct ~ a/(1+(b * exp(-c*(wtAvgRivDist-100)/400))) + d, data = rivBinDataRestSub, 
                         start = list(a = 10, b = 10, c = 10, d = 80), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticRestPctAIC <- AIC(logisticRestPct)
predData$logisticRestPct <- predict(logisticRestPct, newdata = predData)

logisticRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Logistic Model (AIC = ", round(logisticRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticRestPct))


# segmented
segmentedRestPct <- segmented(linearRestPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedRestPctAIC<- AIC(segmentedRestPct)
predData$segmentedRestPct <- predict(segmentedRestPct, newdata = predData)

segmentedRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Segmented Model (AIC = ", round(segmentedRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedRestPct))



# stepwise
stepwiseRestPct <- chngptm(
  formula.1 = wtAvgRestPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataRestSub
)
stepwiseRestPctAIC <- AIC(stepwiseRestPct)
predData$stepwiseRestPct <- predict(stepwiseRestPct, newdata = predData)

stepwiseRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Percent Time Resting", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsRestPct <- ggarrange(nullRestPctplot, 
                             linearRestPctplot, 
                             powerabcRestPctplot, 
                             logisticRestPctplot, 
                             segmentedRestPctplot, 
                             stepwiseRestPctplot,
                             plottitleRestPct, ncol = 3, nrow = 3)


# ggexport(allPlotsRestPct, filename = "rivRestPctModels.pdf", height = 15, width = 15)

########################################################################################
# % Time Moving
rivBinDataMovingSub <- rivBinData %>% 
  filter(!is.na(wtAvgMovingPct))
########################################################################################

# null
nullMovingPct <- lm(data = rivBinDataMovingSub, formula = wtAvgMovingPct ~ 1, weights = nMonkeys)
nullMovingPctAIC <- AIC(nullMovingPct)
predData$nullMovingPct <- predict(nullMovingPct, newdata = predData)

nullMovingPctplot <- ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Null Model (AIC = ", round(nullMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullMovingPct))

# linear
linearMovingPct <- lm(data = rivBinDataMovingSub, formula = wtAvgMovingPct ~ wtAvgRivDist, weights = nMonkeys)
linearMovingPctAIC <- AIC(linearMovingPct)
predData$linearMovingPct <- predict(linearMovingPct, newdata = predData)

linearMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Linear Model (AIC = ", round(linearMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearMovingPct))


# power 
powerabcMovingPct <- nlsLM(wtAvgMovingPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataMovingSub, 
                           start = list(a = 0, b = 1, c = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcMovingPctAIC <- AIC(powerabcMovingPct)
predData$powerabcMovingPct <- predict(powerabcMovingPct, newdata = predData)

powerabcMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Power Model (AIC = ", round(powerabcMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcMovingPct))



# logistic
logisticMovingPct <- nlsLM(wtAvgMovingPct ~ a/(1+(b * exp(-c*(wtAvgRivDist-300)/100))) + d, data = rivBinDataMovingSub, 
                           start = list(a = 10, b = 10, c = 10, d = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticMovingPctAIC <- AIC(logisticMovingPct)
predData$logisticMovingPct <- predict(logisticMovingPct, newdata = predData)

logisticMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Logistic Model (AIC = ", round(logisticMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticMovingPct))


# segmented
segmentedMovingPct <- segmented(linearMovingPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedMovingPctAIC<- AIC(segmentedMovingPct)
predData$segmentedMovingPct <- predict(segmentedMovingPct, newdata = predData)


logLik(segmentedMovingPct)
logLik(nullMovingPct)
nrow(rivBinDataMovingSub)

movingPR2 <- 1-exp((2/32)*(-90.2783-(-84.87355)))
movingPR2

segmentedMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Segmented Model (AIC = ", round(segmentedMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedMovingPct))



# stepwise
stepwiseMovingPct <- chngptm(
  formula.1 = wtAvgMovingPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataMovingSub
)
stepwiseMovingPctAIC <- AIC(stepwiseMovingPct)
predData$stepwiseMovingPct <- predict(stepwiseMovingPct, newdata = predData)

stepwiseMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Percent Time Moving", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsMovingPct <- ggarrange(nullMovingPctplot, 
                               linearMovingPctplot, 
                               powerabcMovingPctplot, 
                               logisticMovingPctplot, 
                               segmentedMovingPctplot, 
                               stepwiseMovingPctplot,
                               plottitleMovingPct, ncol = 3, nrow = 3)


#ggexport(allPlotsMovingPct, filename = "rivMovingPctModels.pdf", height = 15, width = 15)


########################################################################################
# % Time Feeding
rivBinDataFeedingSub <- rivBinData %>% 
  filter(!is.na(wtAvgFeedingPct))
########################################################################################

# null
nullFeedingPct <- lm(data = rivBinDataFeedingSub, formula = wtAvgFeedingPct ~ 1, weights = nMonkeys)
nullFeedingPctAIC <- AIC(nullFeedingPct)
predData$nullFeedingPct <- predict(nullFeedingPct, newdata = predData)

nullFeedingPctplot <- ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Null Model (AIC = ", round(nullFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullFeedingPct))

# linear
linearFeedingPct <- lm(data = rivBinDataFeedingSub, formula = wtAvgFeedingPct ~ wtAvgRivDist, weights = nMonkeys)
linearFeedingPctAIC <- AIC(linearFeedingPct)
predData$linearFeedingPct <- predict(linearFeedingPct, newdata = predData)

linearFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Linear Model (AIC = ", round(linearFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearFeedingPct))


# power 
powerabcFeedingPct <- nlsLM(wtAvgFeedingPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataFeedingSub, 
                            start = list(a = 0, b = 1, c = 10), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcFeedingPctAIC <- AIC(powerabcFeedingPct)
predData$powerabcFeedingPct <- predict(powerabcFeedingPct, newdata = predData)

powerabcFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Power Model (AIC = ", round(powerabcFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcFeedingPct))



# logistic
logisticFeedingPct <- nlsLM(wtAvgFeedingPct ~ a/(1+(b * exp(-c*(wtAvgRivDist)/400))) + d, data = rivBinDataFeedingSub, 
                            start = list(a = 10, b = 10, c = 10, d = 8), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticFeedingPctAIC <- AIC(logisticFeedingPct)
predData$logisticFeedingPct <- predict(logisticFeedingPct, newdata = predData)

logisticFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Logistic Model (AIC = ", round(logisticFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticFeedingPct))


# segmented
segmentedFeedingPct <- segmented(linearFeedingPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedFeedingPctAIC<- AIC(segmentedFeedingPct)
predData$segmentedFeedingPct <- predict(segmentedFeedingPct, newdata = predData)

segmentedFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Segmented Model (AIC = ", round(segmentedFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedFeedingPct))



# stepwise
stepwiseFeedingPct <- chngptm(
  formula.1 = wtAvgFeedingPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataFeedingSub
)
stepwiseFeedingPctAIC <- AIC(stepwiseFeedingPct)
predData$stepwiseFeedingPct <- predict(stepwiseFeedingPct, newdata = predData)

stepwiseFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Percent Time Feeding", 
           hjust = 0.5, vjust = 0, size = 5)

allPlotsFeedingPct <- ggarrange(nullFeedingPctplot, 
                                linearFeedingPctplot, 
                                powerabcFeedingPctplot, 
                                logisticFeedingPctplot, 
                                segmentedFeedingPctplot, 
                                stepwiseFeedingPctplot,
                                plottitleFeedingPct, ncol = 3, nrow = 3)


# ggexport(allPlotsFeedingPct, filename = "rivFeedingPctModels.pdf", height = 15, width = 15)



########################################################################################
# NumNN
rivBinDataNumNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgNumNN))
########################################################################################

# null
nullNumNN <- lm(data = rivBinDataNumNNSub, formula = wtAvgNumNN ~ 1, weights = nMonkeys)
nullNumNNAIC <- AIC(nullNumNN)
predData$nullNumNN <- predict(nullNumNN, newdata = predData)

nullNumNNplot <- ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullNumNN))

# linear
linearNumNN <- lm(data = rivBinDataNumNNSub, formula = wtAvgNumNN ~ wtAvgRivDist, weights = nMonkeys)
linearNumNNAIC <- AIC(linearNumNN)
predData$linearNumNN <- predict(linearNumNN, newdata = predData)

linearNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearNumNN))


# power 
powerabcNumNN <- nlsLM(wtAvgNumNN ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataNumNNSub, 
                       start = list(a = 0, b = 1, c = 1), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcNumNNAIC <- AIC(powerabcNumNN)
predData$powerabcNumNN <- predict(powerabcNumNN, newdata = predData)

powerabcNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Power Model (AIC = ", round(powerabcNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcNumNN))





# logistic
logisticNumNN <- nlsLM(wtAvgNumNN ~ a/(1+(b * exp(-c*(wtAvgRivDist)/400))) + d, data = rivBinDataNumNNSub, 
                       start = list(a = 10, b = 10, c = 10, d = 1), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticNumNNAIC <- AIC(logisticNumNN)
predData$logisticNumNN <- predict(logisticNumNN, newdata = predData)

logisticNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticNumNN))


numnnPR2 <- nagelkerke(logisticNumNN, null = nullNumNN)
numnnPR2 <- numnnPR2$Pseudo.R.squared.for.model.vs.null

# segmented
segmentedNumNN <- segmented(linearNumNN, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedNumNNAIC<- AIC(segmentedNumNN)
predData$segmentedNumNN <- predict(segmentedNumNN, newdata = predData)

segmentedNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNumNN))



# stepwise
stepwiseNumNN <- chngptm(
  formula.1 = wtAvgNumNN ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataNumNNSub
)
stepwiseNumNNAIC <- AIC(stepwiseNumNN)
predData$stepwiseNumNN <- predict(stepwiseNumNN, newdata = predData)

stepwiseNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River # of Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNumNN <- ggarrange(nullNumNNplot, 
                           linearNumNNplot, 
                           powerabcNumNNplot, 
                           logisticNumNNplot, 
                           segmentedNumNNplot, 
                           stepwiseNumNNplot,
                           plottitleNumNN, ncol = 3, nrow = 3)


# ggexport(allPlotsNumNN, filename = "rivNumNNModels.pdf", height = 15, width = 15)


########################################################################################
# DistNN
rivBinDataDistNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgDistNN))
########################################################################################

# null
nullDistNN <- lm(data = rivBinDataDistNNSub, formula = wtAvgDistNN ~ 1, weights = nMonkeys)
nullDistNNAIC <- AIC(nullDistNN)
predData$nullDistNN <- predict(nullDistNN, newdata = predData)

nullDistNNplot <- ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullDistNN))

# linear
linearDistNN <- lm(data = rivBinDataDistNNSub, formula = wtAvgDistNN ~ wtAvgRivDist, weights = nMonkeys)
linearDistNNAIC <- AIC(linearDistNN)
predData$linearDistNN <- predict(linearDistNN, newdata = predData)


distnnPR2 <- nagelkerke(linearDistNN)
distnnPR2 <- distnnPR2$Pseudo.R.squared.for.model.vs.null

linearDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearDistNN))


# power 
powerabcDistNN <- nlsLM(wtAvgDistNN ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataDistNNSub, 
                        start = list(a = 0, b = 1, c = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcDistNNAIC <- AIC(powerabcDistNN)
predData$powerabcDistNN <- predict(powerabcDistNN, newdata = predData)

powerabcDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Power Model (AIC = ", round(powerabcDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcDistNN))




# logistic

logisticDistNN <- nlsLM(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBinDataDistNNSub, 
                        start = list(a = 1.5, b = 130, c = 4.5, d = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticDistNNAIC <- AIC(logisticDistNN)
predData$logisticDistNN <- predict(logisticDistNN, newdata = predData)

logisticDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticDistNN))




# segmented
segmentedDistNN <- segmented(linearDistNN, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedDistNNAIC<- AIC(segmentedDistNN)
predData$segmentedDistNN <- predict(segmentedDistNN, newdata = predData)

segmentedDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedDistNN))



# stepwise
stepwiseDistNN <- chngptm(
  formula.1 = wtAvgDistNN ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataDistNNSub
)
stepwiseDistNNAIC <- AIC(stepwiseDistNN)
predData$stepwiseDistNN <- predict(stepwiseDistNN, newdata = predData)

stepwiseDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Distance from Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsDistNN <- ggarrange(nullDistNNplot, 
                            linearDistNNplot, 
                            powerabcDistNNplot, 
                            logisticDistNNplot, 
                            segmentedDistNNplot, 
                            stepwiseDistNNplot,
                            plottitleDistNN, ncol = 3, nrow = 3)


#ggexport(allPlotsDistNN, filename = "rivDistNNModels.pdf", height = 15, width = 15)

