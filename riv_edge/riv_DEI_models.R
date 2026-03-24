# 3/23/26 
# Lowest AIC Models Selection (Riv)
library(tidyverse)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)
library(investr)
library(msm)



rivBinData <- read.csv("rivBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgRivDist = seq(min(rivBinData$wtAvgRivDist, na.rm = T),
                      max(rivBinData$wtAvgRivDist, na.rm = T),
                      length.out = 200)
)


axis_theme <- theme(
  axis.title = element_text(size = 16),  # axis labels
  axis.text  = element_text(size = 12)   # tick labels
)

spacing_theme <- theme(
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # units are points
)


########################################################################################
# DistNN
rivBinDataDistNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgDistNN))
########################################################################################

# Logistic AIC 74.95
logisticDistNN <- nlsLM(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBinDataDistNNSub, 
                        start = list(a = 1.5, b = 130, c = 4.5, d = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
logisticDistNNAIC <- AIC(logisticDistNN)
predData$logisticDistNN <- predict(logisticDistNN, newdata = predData)




# model coefficients to find point of inflection for logistic model
DistNNCoefs <- coef(logisticDistNN)
distb <- DistNNCoefs[["b"]]
distc <- DistNNCoefs[["c"]]
dista <- DistNNCoefs[["a"]]
distd <- DistNNCoefs[["d"]]

distd

distd + dista


vcovMat <- vcov(logisticDistNN)



# lower asymptote d
se_lower_dm <- deltamethod(~ x4,
                           mean = DistNNCoefs,
                           cov  = vcovMat)
lower_est <- distd
lower_CI  <- lower_est + c(-1.96, 1.96) * se_lower_dm

# upper asymptote a + d
se_upper_dm <- deltamethod(~ x1 + x4,
                           mean = DistNNCoefs,
                           cov  = vcovMat)
upper_est <- distd + dista
upper_CI  <- upper_est + c(-1.96, 1.96) * se_upper_dm






# Delta method
DistNNse <- deltamethod(~ (100/x3) * log(x2), 
                        mean = DistNNCoefs, 
                        cov = vcovMat)




DistNNPE <- ((log(distb)*100)/distc) # Point of inflection calculation from model coefficients
DistNNlower <- DistNNPE - 1.96*DistNNse
DistNNupper <- DistNNPE + 1.96*DistNNse

DistNNciLabel <- paste0("95% CI = (", round(DistNNlower, 1), ", ", round(DistNNupper, 1), ")")
DistNNpeLabel <- paste0("Point estimate = ", round(DistNNPE, 1))

DistNNciBand <- data.frame(
  xmin = DistNNlower,
  xmax = DistNNupper,
  ymin = -Inf,
  ymax = Inf,
  type = DistNNciLabel
)

DistNNpointLine <- data.frame(
  xintercept = DistNNPE,
  type = DistNNpeLabel
)




logisticDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance to Anthropogenic Edge (m)", 
       y = "Mean Distance to Nearest Neighbors"
       # title = paste0("Resting % (Logistic AIC = ", round(logisticRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticDistNN)) +
  geom_rect(data = DistNNciBand,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = DistNNpointLine,
             aes(xintercept = xintercept, color = type),
             linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = setNames("red", DistNNciLabel)) +
  scale_color_manual(values = setNames("red", DistNNpeLabel)) +
  labs(fill = NULL, color = NULL) +
  theme(
    # legend.position = "bottom",               
    # legend.justification = "center",          
    # legend.direction = "horizontal",          
    # legend.box = "horizontal",                
    # legend.background = element_blank(),
    # legend.key = element_blank()
    legend.position = "none"
  )


logisticDistNNplot <- logisticDistNNplot + axis_theme + spacing_theme


########################################################################################
# NumNN
rivBinDataNumNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgNumNN))
########################################################################################

# segmented
linearNumNN <- lm(data = rivBinDataNumNNSub, formula = wtAvgNumNN ~ wtAvgRivDist, weights = nMonkeys)
segmentedNumNN <- segmented(linearNumNN, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedNumNNAIC<- AIC(segmentedNumNN)
predData$segmentedNumNN <- predict(segmentedNumNN, newdata = predData)




NumNNPE <- confint(segmentedNumNN)[1]
NumNNlower <- confint(segmentedNumNN)[2]
NumNNupper <- confint(segmentedNumNN)[3]



NumNNciLabel <- paste0("95% CI = (", round(NumNNlower, 1), ", ", round(NumNNupper, 1), ")")
NumNNpeLabel <- paste0("Point estimate = ", round(NumNNPE, 1))

NumNNciBand <- data.frame(
  xmin = NumNNlower,
  xmax = NumNNupper,
  ymin = -Inf,
  ymax = Inf,
  type = NumNNciLabel
)

NumNNpointLine <- data.frame(
  xintercept = NumNNPE,
  type = NumNNpeLabel
)




segmentedNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance to Anthropogenic Edge (m)", 
       y = "Mean Number of Nearest Neighbors" 
       # title = paste0("Moving % (Segmented AIC = ", round(segmentedMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNumNN)) +
  geom_rect(data = NumNNciBand,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = NumNNpointLine,
             aes(xintercept = xintercept, color = type),
             linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = setNames("red", NumNNciLabel)) +
  scale_color_manual(values = setNames("red", NumNNpeLabel)) +
  labs(fill = NULL, color = NULL) +
  theme(
    # legend.position = "bottom",               
    # legend.justification = "center",          
    # legend.direction = "horizontal",          
    # legend.box = "horizontal",                
    # legend.background = element_blank(),
    # legend.key = element_blank()
    legend.position = "none"
  )


segmentedNumNNplot <- segmentedNumNNplot + axis_theme + spacing_theme


########################################################################################
# % Time Feeding
rivBinDataFeedingSub <- rivBinData %>% 
  filter(!is.na(wtAvgFeedingPct))
########################################################################################

# linear
linearFeedingPct <- lm(data = rivBinDataFeedingSub, formula = wtAvgFeedingPct ~ wtAvgRivDist, weights = nMonkeys)
linearFeedingPctAIC <- AIC(linearFeedingPct)
predData$linearFeedingPct <- predict(linearFeedingPct, newdata = predData)




# DECREASING

# max and min observed x
FeedingPctxmin <- min(rivBinDataFeedingSub$wtAvgRivDist, na.rm = TRUE)
FeedingPctxmax <- max(rivBinDataFeedingSub$wtAvgRivDist, na.rm = TRUE)


# model predicted y at that x
FeedingPctymax <- predict(linearFeedingPct,
                          newdata = data.frame(wtAvgRivDist = FeedingPctxmin))
FeedingPctymin <- predict(linearFeedingPct,
                          newdata = data.frame(wtAvgRivDist = FeedingPctxmax))
# 
FeedingPctydei <- FeedingPctymax - (2/3)*(FeedingPctymax - FeedingPctymin)

FeedingPctinvest <- invest(linearFeedingPct,
                           seed = 123,
                           y0 = FeedingPctydei,
                           interval = "percentile",
                           nsim = 10000,  
                           boot.type = "nonparametric", 
                           progress = TRUE)


FeedingPctPE <- as.numeric(FeedingPctinvest$estimate)
FeedingPctlower <- as.numeric(FeedingPctinvest$lower)
FeedingPctupper <- as.numeric(FeedingPctinvest$upper)

FeedingPctciLabel <- paste0("95% CI = (", round(FeedingPctlower, 1), ", ", round(FeedingPctupper, 1), ")")
FeedingPctpeLabel <- paste0("Point estimate = ", round(FeedingPctPE, 1))

FeedingPctciBand <- data.frame(
  xmin = FeedingPctlower,
  xmax = FeedingPctupper,
  ymin = -Inf,
  ymax = Inf,
  type = FeedingPctciLabel
)

FeedingPctpointLine <- data.frame(
  xintercept = FeedingPctPE,
  type = FeedingPctpeLabel
)




linearFeedingPctplot <-ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance to Anthropogenic Edge (m)", 
       y = "Mean Percent Time Spent Feeding" 
       # title = paste0("Feeding % (Linear AIC = ", round(linearFeedingPctAIC, 2),")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearFeedingPct)) +
  geom_rect(data = FeedingPctciBand,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = FeedingPctpointLine,
             aes(xintercept = xintercept, color = type),
             linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = setNames("red", FeedingPctciLabel)) +
  scale_color_manual(values = setNames("red", FeedingPctpeLabel)) +
  labs(fill = NULL, color = NULL) + 
  theme(
    # legend.position = "bottom",               
    # legend.justification = "center",          
    # legend.direction = "horizontal",          
    # legend.box = "horizontal",                
    # legend.background = element_blank(),
    # legend.key = element_blank()
    legend.position = "none"
  )

linearFeedingPctplot <- linearFeedingPctplot + axis_theme + spacing_theme


########################################################################################
# % Time Moving
rivBinDataMovingSub <- rivBinData %>% 
  filter(!is.na(wtAvgMovingPct))
########################################################################################

# segmented
linearMovingPct <- lm(data = rivBinDataMovingSub, formula = wtAvgMovingPct ~ wtAvgRivDist, weights = nMonkeys)
segmentedMovingPct <- segmented(linearMovingPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedMovingPctAIC<- AIC(segmentedMovingPct)
predData$segmentedMovingPct <- predict(segmentedMovingPct, newdata = predData)




MovingPctPE <- confint(segmentedMovingPct)[1]
MovingPctlower <- confint(segmentedMovingPct)[2]
MovingPctupper <- confint(segmentedMovingPct)[3]



MovingPctciLabel <- paste0("95% CI = (", round(MovingPctlower, 1), ", ", round(MovingPctupper, 1), ")")
MovingPctpeLabel <- paste0("Point estimate = ", round(MovingPctPE, 1))

MovingPctciBand <- data.frame(
  xmin = MovingPctlower,
  xmax = MovingPctupper,
  ymin = -Inf,
  ymax = Inf,
  type = MovingPctciLabel
)

MovingPctpointLine <- data.frame(
  xintercept = MovingPctPE,
  type = MovingPctpeLabel
)




segmentedMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance to River Edge (m)", 
       y = "Mean Percent Time Spent Moving" 
       # title = paste0("Moving % (Segmented AIC = ", round(segmentedMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedMovingPct)) +
  geom_rect(data = MovingPctciBand,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = MovingPctpointLine,
             aes(xintercept = xintercept, color = type),
             linetype = "dashed", size = 0.8) +
  scale_fill_manual(values = setNames("red", MovingPctciLabel)) +
  scale_color_manual(values = setNames("red", MovingPctpeLabel)) +
  labs(fill = NULL, color = NULL) +
  theme(
    # legend.position = "bottom",               
    # legend.justification = "center",          
    # legend.direction = "horizontal",          
    # legend.box = "horizontal",                
    # legend.background = element_blank(),
    # legend.key = element_blank()
    legend.position = "none"
  )


segmentedMovingPctplot <- segmentedMovingPctplot + axis_theme + spacing_theme


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
  labs(x = "Distance to Anthropogenic Edge (m)", 
       y = "Mean Percent Time Spent Resting" 
       # title = paste0("Moving % (Null AIC = ", round(nullMovingPctAIC, 2), ")"),
       # caption = "No DEI Effects"
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullRestPct)) +
  theme(
    # legend.position = "bottom",
    # legend.box = "horizontal",
    # legend.key = element_blank(),
    # legend.background = element_blank(),                           # remove legend
    # plot.caption = element_text(hjust = 0.5, size = 10)  
    legend.position = "none"
  )

nullRestPctplot <- nullRestPctplot + axis_theme + spacing_theme







allDEIplotsAnth <- ggarrange(logisticDistNNplot,
                             segmentedNumNNplot,
                             linearFeedingPctplot,
                             segmentedMovingPctplot,
                             nullRestPctplot,
                             ncol = 2, nrow = 3,
                             labels   = c("a", "b", "c", "d", "e"),        # panel letters
                             label.x  = 0.02,                               # a little inset from left
                             label.y  = 0.98,                               # near the top
                             hjust    = 0,                                  # left aligned
                             vjust    = 1,                                  # top aligned
                             font.label = list(size = 20, face = "bold"))


allDEIplotsAnth

# save.image(file = "anthDEImodels.RData")


ggexport(allDEIplotsAnth, filename = "allDEIplotsAnth.pdf", height = 15, width = 11)





