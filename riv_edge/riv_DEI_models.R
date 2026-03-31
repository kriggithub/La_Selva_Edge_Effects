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
library(rcompanion)



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

# Linear AIC 106.25
linearDistNN <- lm(data = rivBinDataDistNNSub, formula = wtAvgDistNN ~ wtAvgRivDist, weights = nMonkeys)
linearDistNNAIC <- AIC(linearDistNN)
predData$linearDistNN <- predict(linearDistNN, newdata = predData)


# DECREASING

# max and min observed x
DistNNxmin <- min(rivBinDataDistNNSub$wtAvgRivDist, na.rm = TRUE)
DistNNxmax <- max(rivBinDataDistNNSub$wtAvgRivDist, na.rm = TRUE)


# model predicted y at that x
DistNNymax <- predict(linearDistNN,
                          newdata = data.frame(wtAvgRivDist = DistNNxmin))
DistNNymin <- predict(linearDistNN,
                          newdata = data.frame(wtAvgRivDist = DistNNxmax))
# 
DistNNydei <- DistNNymax - (2/3)*(DistNNymax - DistNNymin)

DistNNinvest <- invest(linearDistNN,
                           seed = 123,
                           y0 = DistNNydei,
                           interval = "percentile",
                           nsim = 10000,  
                           boot.type = "nonparametric", 
                           progress = TRUE)


DistNNPE <- as.numeric(DistNNinvest$estimate)
DistNNlower <- as.numeric(DistNNinvest$lower)
DistNNupper <- as.numeric(DistNNinvest$upper)

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


#Pseudo
DistNNPR2 <- nagelkerke(linearDistNN)
DistNNPR2 <- DistNNPR2$Pseudo.R.squared.for.model.vs.null



linearDistNNplot <-ggplot(rivBinDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance to Riparian Edge (m)", 
       y = "Mean Distance to Nearest Neighbors" 
       # title = paste0("DistNN % (Linear AIC = ", round(linearDistNNAIC, 2),")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearDistNN)) +
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

linearDistNNplot <- linearDistNNplot + axis_theme + spacing_theme








########################################################################################
# NumNN
rivBinDataNumNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgNumNN))
########################################################################################

# Segmented AIC 87.93

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


# Calculate Cox & Snell Pseudo R2
# numNNPR2 <- 1 - exp((2/n) * (lnL0 - lnL1))


nullNumNN <- lm(data = rivBinDataNumNNSub, formula = wtAvgNumNN ~ 1, weights = nMonkeys)


logLik(segmentedNumNN)
logLik(nullNumNN)
nrow(rivBinDataNumNNSub)

numNNPR2 <- 1-exp((2/35)*(-44.61018-(-38.96345)))
numNNPR2




segmentedNumNNplot <-ggplot(rivBinDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance to Riparian Edge (m)", 
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

# null AIC 267.18
nullFeedingPct <- lm(data = rivBinDataFeedingSub, formula = wtAvgFeedingPct ~ 1, weights = nMonkeys)
nullFeedingPctAIC <- AIC(nullFeedingPct)
predData$nullFeedingPct <- predict(nullFeedingPct, newdata = predData)

nullFeedingPctplot <- ggplot(rivBinDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance to Riparian Edge (m)", 
       y = "Mean Percent Time Spent Feeding" 
       # title = paste0("Moving % (Null AIC = ", round(nullMovingPctAIC, 2), ")"),
       # caption = "No DEI Effects"
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullFeedingPct)) +
  theme(
    # legend.position = "bottom",
    # legend.box = "horizontal",
    # legend.key = element_blank(),
    # legend.background = element_blank(),                           # remove legend
    # plot.caption = element_text(hjust = 0.5, size = 10)  
    legend.position = "none"
  )



nullFeedingPctplot <- nullFeedingPctplot + axis_theme + spacing_theme



########################################################################################
# % Time Moving
rivBinDataMovingSub <- rivBinData %>% 
  filter(!is.na(wtAvgMovingPct))
########################################################################################

# segmented AIC 235.19
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

# Calculate Cox & Snell Pseudo R2
# numNNPR2 <- 1 - exp((2/n) * (lnL0 - lnL1))


nullMovingPct <- lm(data = rivBinDataMovingSub, formula = wtAvgMovingPct ~ 1, weights = nMonkeys)


logLik(segmentedMovingPct)
logLik(nullMovingPct)
nrow(rivBinDataMovingSub)

movingPR2 <- 1-exp((2/36)*(-116.2742-(-112.5943)))
movingPR2




segmentedMovingPctplot <-ggplot(rivBinDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance to Riparian Edge (m)", 
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
  labs(x = "Distance to Riparian Edge (m)", 
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







allDEIplotsRiv <- ggarrange(linearDistNNplot,
                             segmentedNumNNplot,
                             nullFeedingPctplot,
                             segmentedMovingPctplot,
                             nullRestPctplot,
                             ncol = 2, nrow = 3,
                             labels   = c("a", "b", "c", "d", "e"),        # panel letters
                             label.x  = 0.02,                               # a little inset from left
                             label.y  = 0.98,                               # near the top
                             hjust    = 0,                                  # left aligned
                             vjust    = 1,                                  # top aligned
                             font.label = list(size = 20, face = "bold"))


allDEIplotsRiv

# save.image(file = "anthDEImodels.RData")


# ggexport(allDEIplotsRiv, filename = "allDEIplotsRiv.pdf", height = 15, width = 11)





