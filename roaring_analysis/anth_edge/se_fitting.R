# 3/10/26
# Kurt Riggin
# Fitting models to binned Anthropogenic edge data
library(tidyverse)
library(ggpubr)
library(segmented)
library(chngpt)
library(minpack.lm)
library(rcompanion)



anthBinData <- read.csv("anth_n_howls_in_bout.csv")
anth_bin_data_n_bouts <- read.csv("anth_roar_bout_per_h.csv")


# create prediction dataframe

predData <- data.frame(
  avg_anth_dist = seq(min(anthBinData$avg_anth_dist, na.rm = T),
                      max(anthBinData$avg_anth_dist, na.rm = T),
                      length.out = 200)
)

pred_data_n_bouts <- data.frame(
  avg_anth_dist = seq(min(anth_bin_data_n_bouts$avg_anth_dist, na.rm = T),
                      max(anth_bin_data_n_bouts$avg_anth_dist, na.rm = T),
                      length.out = 200)
)


########################################################################################
# Bout Length (s)
anthBinDataBoutLength <- anthBinData %>% 
  filter(!is.na(avg_bout_length_s))
########################################################################################

# null
nullBoutLength <- lm(data = anthBinDataBoutLength, formula = avg_bout_length_s ~ 1, weights = 1/(se_bout_length_s)^2)
nullBoutLengthAIC <- AIC(nullBoutLength)
predData$nullBoutLength <- predict(nullBoutLength, newdata = predData)

nullBoutLengthplot <- ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Null Model (AIC = ", round(nullBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullBoutLength))

# linear
linearBoutLength <- lm(data = anthBinDataBoutLength, formula = avg_bout_length_s ~ avg_anth_dist, weights = 1/(se_bout_length_s)^2)
linearBoutLengthAIC <- AIC(linearBoutLength)
predData$linearBoutLength <- predict(linearBoutLength, newdata = predData)

linearBoutLengthplot <-ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Linear Model (AIC = ", round(linearBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearBoutLength))


# power 
powerabcBoutLength <- nlsLM(avg_bout_length_s ~ a * ((avg_anth_dist)^b) + c, data = anthBinDataBoutLength, 
                            start = list(a = 1, b = 1, c = 80), weights = 1/(se_bout_length_s)^2, control = nls.lm.control(maxiter = 1000))
powerabcBoutLengthAIC <- AIC(powerabcBoutLength)
predData$powerabcBoutLength <- predict(powerabcBoutLength, newdata = predData)

powerabcBoutLengthplot <-ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Power Model (AIC = ", round(powerabcBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcBoutLength))




# logistic
logisticBoutLength <- nlsLM(avg_bout_length_s ~ a/(1+(b * exp(-c*(avg_anth_dist-0)/100))) + d, data = anthBinDataBoutLength, 
                            start = list(a = 90, b = 20, c = 2.5, d = 40), weights = 1/(se_bout_length_s)^2, control = nls.lm.control(maxiter = 1000))
logisticBoutLengthAIC <- AIC(logisticBoutLength)
predData$logisticBoutLength <- predict(logisticBoutLength, newdata = predData)

logisticBoutLengthplot <-ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Logistic Model (AIC = ", round(logisticBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticBoutLength))


# segmented
segmentedBoutLength <- segmented(linearBoutLength, seg.Z = ~ avg_anth_dist, psi = 250)
segmentedBoutLengthAIC<- AIC(segmentedBoutLength)
predData$segmentedBoutLength <- predict(segmentedBoutLength, newdata = predData)

segmentedBoutLengthplot <-ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Segmented Model (AIC = ", round(segmentedBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedBoutLength))



# stepwise
stepwiseBoutLength <- chngptm(
  formula.1 = avg_bout_length_s ~ 1,
  formula.2 =  ~ avg_anth_dist,
  type = "step",
  family = "gaussian",
  data = anthBinDataBoutLength
)
stepwiseBoutLengthAIC <- AIC(stepwiseBoutLength)
predData$stepwiseBoutLength <- predict(stepwiseBoutLength, newdata = predData)

stepwiseBoutLengthplot <-ggplot(anthBinDataBoutLength, aes(x = avg_anth_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseBoutLength))


# plot title
plottitleBoutLength <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Bout Length (s)", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsBoutLength <- ggarrange(nullBoutLengthplot, 
                                linearBoutLengthplot, 
                                powerabcBoutLengthplot, 
                                logisticBoutLengthplot, 
                                segmentedBoutLengthplot, 
                                stepwiseBoutLengthplot,
                                plottitleBoutLength, ncol = 3, nrow = 3)

# ggexport(allPlotsBoutLength, filename = "anthBoutLengthModels.pdf", height = 15, width = 15)


########################################################################################
# N Howls in Bout
anthBinDataNbouts <- anthBinData %>% 
  filter(!is.na(avg_n_howls_in_bout))
########################################################################################

# null
nullNbouts <- lm(data = anthBinDataNbouts, formula = avg_n_howls_in_bout ~ 1, weights = 1/(se_n_howls_in_bout)^2)
nullNboutsAIC <- AIC(nullNbouts)
predData$nullNbouts <- predict(nullNbouts, newdata = predData)

nullNboutsplot <- ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Null Model (AIC = ", round(nullNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullNbouts))

# linear
linearNbouts <- lm(data = anthBinDataNbouts, formula = avg_n_howls_in_bout ~ avg_anth_dist, weights = 1/(se_n_howls_in_bout)^2)
linearNboutsAIC <- AIC(linearNbouts)
predData$linearNbouts <- predict(linearNbouts, newdata = predData)

linearNboutsplot <-ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Linear Model (AIC = ", round(linearNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearNbouts))


# power 
powerabcNbouts <- nlsLM(avg_n_howls_in_bout ~ a * ((avg_anth_dist)^b) + c, data = anthBinDataNbouts, 
                        start = list(a = 1, b = 1, c = 1), weights = 1/(se_n_howls_in_bout)^2, control = nls.lm.control(maxiter = 1000))
powerabcNboutsAIC <- AIC(powerabcNbouts)
predData$powerabcNbouts <- predict(powerabcNbouts, newdata = predData)

powerabcNboutsplot <-ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Power Model (AIC = ", round(powerabcNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcNbouts))




# logistic
logisticNbouts <- nlsLM(avg_n_howls_in_bout ~ a/(1+(b * exp(-c*(avg_anth_dist-0)/100))) + d, data = anthBinDataNbouts, 
                        start = list(a = 0, b = 20, c = 2.5, d = 40), weights = 1/(se_n_howls_in_bout)^2, control = nls.lm.control(maxiter = 1000))
logisticNboutsAIC <- AIC(logisticNbouts)
predData$logisticNbouts <- predict(logisticNbouts, newdata = predData)

logisticNboutsplot <-ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Logistic Model (AIC = ", round(logisticNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticNbouts))


# segmented
segmentedNbouts <- segmented(linearNbouts, seg.Z = ~ avg_anth_dist, psi = 250)
segmentedNboutsAIC<- AIC(segmentedNbouts)
predData$segmentedNbouts <- predict(segmentedNbouts, newdata = predData)

segmentedNboutsplot <-ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Segmented Model (AIC = ", round(segmentedNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNbouts))



# stepwise
stepwiseNbouts <- chngptm(
  formula.1 = avg_n_howls_in_bout ~ 1,
  formula.2 =  ~ avg_anth_dist,
  type = "step",
  family = "gaussian",
  data = anthBinDataNbouts
)
stepwiseNboutsAIC <- AIC(stepwiseNbouts)
predData$stepwiseNbouts <- predict(stepwiseNbouts, newdata = predData)

stepwiseNboutsplot <-ggplot(anthBinDataNbouts, aes(x = avg_anth_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseNbouts))


# plot title
plottitleNbouts <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Number of Roars in Bout", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNbouts <- ggarrange(nullNboutsplot, 
                            linearNboutsplot, 
                            powerabcNboutsplot, 
                            logisticNboutsplot, 
                            segmentedNboutsplot, 
                            stepwiseNboutsplot,
                            plottitleNbouts, ncol = 3, nrow = 3)

# ggexport(allPlotsNbouts, filename = "anthNroarsModels.pdf", height = 15, width = 15)



#######################################################################################
# Howls per min
anthBinDataHowlsMin <- anthBinData %>% 
  filter(!is.na(avg_howl_per_min))
########################################################################################

# null
nullHowlsMin <- lm(data = anthBinDataHowlsMin, formula = avg_howl_per_min ~ 1, weights = 1/(se_howl_per_min)^2)
nullHowlsMinAIC <- AIC(nullHowlsMin)
predData$nullHowlsMin <- predict(nullHowlsMin, newdata = predData)

nullHowlsMinplot <- ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Null Model (AIC = ", round(nullHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullHowlsMin))

# linear
linearHowlsMin <- lm(data = anthBinDataHowlsMin, formula = avg_howl_per_min ~ avg_anth_dist, weights = 1/(se_howl_per_min)^2)
linearHowlsMinAIC <- AIC(linearHowlsMin)
predData$linearHowlsMin <- predict(linearHowlsMin, newdata = predData)

linearHowlsMinplot <-ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Linear Model (AIC = ", round(linearHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearHowlsMin))


# power 
powerabcHowlsMin <- nlsLM(avg_howl_per_min ~ a * ((avg_anth_dist)^b) + c, data = anthBinDataHowlsMin, 
                          start = list(a = 1, b = 1, c = 80), weights = 1/(se_howl_per_min)^2, control = nls.lm.control(maxiter = 1000))
powerabcHowlsMinAIC <- AIC(powerabcHowlsMin)
predData$powerabcHowlsMin <- predict(powerabcHowlsMin, newdata = predData)

powerabcHowlsMinplot <-ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Power Model (AIC = ", round(powerabcHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcHowlsMin))




# logistic
logisticHowlsMin <- nlsLM(avg_howl_per_min ~ a/(1+(b * exp(-c*(avg_anth_dist-0)/100))) + d, data = anthBinDataHowlsMin, 
                          start = list(a = 90, b = 20, c = 2.5, d = 40), weights = 1/(se_howl_per_min)^2, control = nls.lm.control(maxiter = 1000))
logisticHowlsMinAIC <- AIC(logisticHowlsMin)
predData$logisticHowlsMin <- predict(logisticHowlsMin, newdata = predData)

logisticHowlsMinplot <-ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Logistic Model (AIC = ", round(logisticHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticHowlsMin))


# segmented
segmentedHowlsMin <- segmented(linearHowlsMin, seg.Z = ~ avg_anth_dist, psi = 250)
segmentedHowlsMinAIC<- AIC(segmentedHowlsMin)
predData$segmentedHowlsMin <- predict(segmentedHowlsMin, newdata = predData)

segmentedHowlsMinplot <-ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Segmented Model (AIC = ", round(segmentedHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedHowlsMin))



# stepwise
stepwiseHowlsMin <- chngptm(
  formula.1 = avg_howl_per_min ~ 1,
  formula.2 =  ~ avg_anth_dist,
  type = "step",
  family = "gaussian",
  data = anthBinDataHowlsMin
)
stepwiseHowlsMinAIC <- AIC(stepwiseHowlsMin)
predData$stepwiseHowlsMin <- predict(stepwiseHowlsMin, newdata = predData)

stepwiseHowlsMinplot <-ggplot(anthBinDataHowlsMin, aes(x = avg_anth_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseHowlsMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseHowlsMin))


# plot title
plottitleHowlsMin <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Howls per minute", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsHowlsMin <- ggarrange(nullHowlsMinplot, 
                              linearHowlsMinplot, 
                              powerabcHowlsMinplot, 
                              logisticHowlsMinplot, 
                              segmentedHowlsMinplot, 
                              stepwiseHowlsMinplot,
                              plottitleHowlsMin, ncol = 3, nrow = 3)

# ggexport(allPlotsHowlsMin, filename = "anthHowlsMinModels.pdf", height = 15, width = 15)








########################################################################################
# N bouts per hour
anth_bin_data_n_bouts <- anth_bin_data_n_bouts %>% 
  filter(!is.na(se_roar_bout_per_h), se_roar_bout_per_h > 0)
########################################################################################

# null
null_n_bouts <- lm(data = anth_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ 1, weights = 1/(se_roar_bout_per_h)^2)
null_n_boutsAIC <- AIC(null_n_bouts)
pred_data_n_bouts$null_n_bouts <- predict(null_n_bouts, newdata = pred_data_n_bouts)

null_n_boutsplot <- ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Null Model (AIC = ", round(null_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = null_n_bouts))

# linear
linear_n_bouts <- lm(data = anth_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ avg_anth_dist, weights = 1/(se_roar_bout_per_h)^2)
linear_n_boutsAIC <- AIC(linear_n_bouts)
pred_data_n_bouts$linear_n_bouts <- predict(linear_n_bouts, newdata = pred_data_n_bouts)

linear_n_boutsplot <-ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Linear Model (AIC = ", round(linear_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = linear_n_bouts))


# power 
powerabc_n_bouts <- nlsLM(avg_roar_bout_per_h ~ a * ((avg_anth_dist)^b) + c, data = anth_bin_data_n_bouts, 
                          start = list(a = 1, b = 1, c = 80), weights = 1/(se_roar_bout_per_h)^2, control = nls.lm.control(maxiter = 1000))
powerabc_n_boutsAIC <- AIC(powerabc_n_bouts)
pred_data_n_bouts$powerabc_n_bouts <- predict(powerabc_n_bouts, newdata = pred_data_n_bouts)

powerabc_n_boutsplot <-ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Power Model (AIC = ", round(powerabc_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = powerabc_n_bouts))




# logistic
logistic_n_bouts <- nlsLM(avg_roar_bout_per_h ~ a/(1+(b * exp(-c*(avg_anth_dist-0)/100))) + d, data = anth_bin_data_n_bouts, 
                          start = list(a = 90, b = 20, c = 2.5, d = 40), weights = 1/(se_roar_bout_per_h)^2, control = nls.lm.control(maxiter = 1000))
logistic_n_boutsAIC <- AIC(logistic_n_bouts)
pred_data_n_bouts$logistic_n_bouts <- predict(logistic_n_bouts, newdata = pred_data_n_bouts)

logistic_n_boutsplot <-ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Logistic Model (AIC = ", round(logistic_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = logistic_n_bouts))


# segmented
segmented_n_bouts <- segmented(linear_n_bouts, seg.Z = ~ avg_anth_dist, psi = 250)
segmented_n_boutsAIC<- AIC(segmented_n_bouts)
pred_data_n_bouts$segmented_n_bouts <- predict(segmented_n_bouts, newdata = pred_data_n_bouts)

segmented_n_boutsplot <-ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Segmented Model (AIC = ", round(segmented_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = segmented_n_bouts))



# stepwise
stepwise_n_bouts <- chngptm(
  formula.1 = avg_roar_bout_per_h ~ 1,
  formula.2 =  ~ avg_anth_dist,
  type = "step",
  family = "gaussian",
  data = anth_bin_data_n_bouts
)
stepwise_n_boutsAIC <- AIC(stepwise_n_bouts)
pred_data_n_bouts$stepwise_n_bouts <- predict(stepwise_n_bouts, newdata = pred_data_n_bouts)

stepwise_n_boutsplot <-ggplot(anth_bin_data_n_bouts, aes(x = avg_anth_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Stepwise Model (AIC = ", round(stepwise_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = stepwise_n_bouts))


# plot title
plottitle_n_bouts <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Number of Bouts per hour", 
           hjust = 0.5, vjust = 0, size = 5)




allPlots_n_bouts <- ggarrange(null_n_boutsplot, 
                              linear_n_boutsplot, 
                              powerabc_n_boutsplot, 
                              logistic_n_boutsplot, 
                              segmented_n_boutsplot, 
                              stepwise_n_boutsplot,
                              plottitle_n_bouts, ncol = 3, nrow = 3)

# ggexport(allPlots_n_bouts, filename = "anthN_boutsModels.pdf", height = 15, width = 15)



########################################################################################
# Howling probability
anth_bin_data_howl_prob <- anth_bin_data_n_bouts %>% 
  filter(!is.na(avg_roar_prob))
########################################################################################

# null
null_howl_prob <- lm(data = anth_bin_data_howl_prob, formula = avg_roar_prob ~ 1, weights = 1/(se_roar_prob)^2)
null_howl_probAIC <- AIC(null_howl_prob)
pred_data_n_bouts$null_howl_prob <- predict(null_howl_prob, newdata = pred_data_n_bouts)

null_howl_probplot <- ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Null Model (AIC = ", round(null_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = null_howl_prob))

# linear
linear_howl_prob <- lm(data = anth_bin_data_howl_prob, formula = avg_roar_prob ~ avg_anth_dist, weights = 1/(se_roar_prob)^2)
linear_howl_probAIC <- AIC(linear_howl_prob)
pred_data_n_bouts$linear_howl_prob <- predict(linear_howl_prob, newdata = pred_data_n_bouts)

linear_howl_probplot <-ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Linear Model (AIC = ", round(linear_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = linear_howl_prob))


# power 
powerabc_howl_prob <- nlsLM(avg_roar_prob ~ a * ((avg_anth_dist)^b) + c, data = anth_bin_data_howl_prob, 
                            start = list(a = 1, b = 1, c = 80), weights = 1/(se_roar_prob)^2, control = nls.lm.control(maxiter = 1000))
powerabc_howl_probAIC <- AIC(powerabc_howl_prob)
pred_data_n_bouts$powerabc_howl_prob <- predict(powerabc_howl_prob, newdata = pred_data_n_bouts)

powerabc_howl_probplot <-ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Power Model (AIC = ", round(powerabc_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = powerabc_howl_prob))




# logistic
logistic_howl_prob <- nlsLM(avg_roar_prob ~ a/(1+(b * exp(-c*(avg_anth_dist-0)/100))) + d, data = anth_bin_data_howl_prob, 
                            start = list(a = 90, b = 20, c = 2.5, d = 40), weights = 1/(se_roar_prob)^2, control = nls.lm.control(maxiter = 1000))
logistic_howl_probAIC <- AIC(logistic_howl_prob)
pred_data_n_bouts$logistic_howl_prob <- predict(logistic_howl_prob, newdata = pred_data_n_bouts)

logistic_howl_probplot <-ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Logistic Model (AIC = ", round(logistic_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = logistic_howl_prob))


# segmented
segmented_howl_prob <- segmented(linear_howl_prob, seg.Z = ~ avg_anth_dist, psi = 250)
segmented_howl_probAIC<- AIC(segmented_howl_prob)
pred_data_n_bouts$segmented_howl_prob <- predict(segmented_howl_prob, newdata = pred_data_n_bouts)

segmented_howl_probplot <-ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Segmented Model (AIC = ", round(segmented_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = segmented_howl_prob))



# stepwise
stepwise_howl_prob <- chngptm(
  formula.1 = avg_roar_prob ~ 1,
  formula.2 =  ~ avg_anth_dist,
  type = "step",
  family = "gaussian",
  data = anth_bin_data_howl_prob
)
stepwise_howl_probAIC <- AIC(stepwise_howl_prob)
pred_data_n_bouts$stepwise_howl_prob <- predict(stepwise_howl_prob, newdata = pred_data_n_bouts)

stepwise_howl_probplot <-ggplot(anth_bin_data_howl_prob, aes(x = avg_anth_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Stepwise Model (AIC = ", round(stepwise_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = stepwise_howl_prob))


# plot title
plottitle_howl_prob <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Probability of Roaring", 
           hjust = 0.5, vjust = 0, size = 5)




allPlots_howl_prob <- ggarrange(null_howl_probplot, 
                                linear_howl_probplot, 
                                powerabc_howl_probplot, 
                                logistic_howl_probplot, 
                                segmented_howl_probplot, 
                                stepwise_howl_probplot,
                                plottitle_howl_prob, ncol = 3, nrow = 3)

# ggexport(allPlots_howl_prob, filename = "anthhowl_probModels.pdf", height = 15, width = 15)



