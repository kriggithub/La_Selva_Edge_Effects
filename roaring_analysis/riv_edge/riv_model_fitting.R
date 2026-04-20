# 3/10/26
# Kurt Riggin
# Fitting models to binned River edge data
library(tidyverse)
library(ggpubr)
library(segmented)
library(chngpt)
library(minpack.lm)
library(rcompanion)



rivBinData <- read.csv("riv_n_howls_in_bout.csv")
riv_bin_data_n_bouts <- read.csv("riv_roar_bout_per_h.csv")



# create prediction dataframe

predData <- data.frame(
  avg_riv_dist = seq(min(rivBinData$avg_riv_dist, na.rm = T),
                      max(rivBinData$avg_riv_dist, na.rm = T),
                      length.out = 200)
)

pred_data_n_bouts <- data.frame(
  avg_riv_dist = seq(min(riv_bin_data_n_bouts$avg_riv_dist, na.rm = T),
                      max(riv_bin_data_n_bouts$avg_riv_dist, na.rm = T),
                      length.out = 200)
)

########################################################################################
# Bout Length (s)
rivBinDataBoutLength <- rivBinData %>% 
  filter(!is.na(avg_bout_length_s))
########################################################################################

# null
nullBoutLength <- lm(data = rivBinDataBoutLength, formula = avg_bout_length_s ~ 1, weights = n_obs)
nullBoutLengthAIC <- AIC(nullBoutLength)
predData$nullBoutLength <- predict(nullBoutLength, newdata = predData)

nullBoutLengthplot <- ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Null Model (AIC = ", round(nullBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullBoutLength))

# linear
linearBoutLength <- lm(data = rivBinDataBoutLength, formula = avg_bout_length_s ~ avg_riv_dist, weights = n_obs)
linearBoutLengthAIC <- AIC(linearBoutLength)
predData$linearBoutLength <- predict(linearBoutLength, newdata = predData)

linearBoutLengthplot <-ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Linear Model (AIC = ", round(linearBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearBoutLength))


# power 
powerabcBoutLength <- nlsLM(avg_bout_length_s ~ a * ((avg_riv_dist)^b) + c, data = rivBinDataBoutLength, 
                        start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
powerabcBoutLengthAIC <- AIC(powerabcBoutLength)
predData$powerabcBoutLength <- predict(powerabcBoutLength, newdata = predData)

powerabcBoutLengthplot <-ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Power Model (AIC = ", round(powerabcBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcBoutLength))




# logistic
logisticBoutLength <- nlsLM(avg_bout_length_s ~ a/(1+(b * exp(-c*(avg_riv_dist-0)/100))) + d, data = rivBinDataBoutLength, 
                        start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
logisticBoutLengthAIC <- AIC(logisticBoutLength)
predData$logisticBoutLength <- predict(logisticBoutLength, newdata = predData)

logisticBoutLengthplot <-ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Logistic Model (AIC = ", round(logisticBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticBoutLength))


# segmented
segmentedBoutLength <- segmented(linearBoutLength, seg.Z = ~ avg_riv_dist, psi = 250)
segmentedBoutLengthAIC<- AIC(segmentedBoutLength)
predData$segmentedBoutLength <- predict(segmentedBoutLength, newdata = predData)

segmentedBoutLengthplot <-ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Bout Length (s)", 
       title = paste0("Segmented Model (AIC = ", round(segmentedBoutLengthAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedBoutLength))



# stepwise
stepwiseBoutLength <- chngptm(
  formula.1 = avg_bout_length_s ~ 1,
  formula.2 =  ~ avg_riv_dist,
  type = "step",
  family = "gaussian",
  data = rivBinDataBoutLength
)
stepwiseBoutLengthAIC <- AIC(stepwiseBoutLength)
predData$stepwiseBoutLength <- predict(stepwiseBoutLength, newdata = predData)

stepwiseBoutLengthplot <-ggplot(rivBinDataBoutLength, aes(x = avg_riv_dist, y = avg_bout_length_s)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_bout_length_s - se_bout_length_s, ymax = avg_bout_length_s + se_bout_length_s)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Mean Bout Length (s)", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsBoutLength <- ggarrange(nullBoutLengthplot, 
                            linearBoutLengthplot, 
                            powerabcBoutLengthplot, 
                            logisticBoutLengthplot, 
                            segmentedBoutLengthplot, 
                            stepwiseBoutLengthplot,
                            plottitleBoutLength, ncol = 3, nrow = 3)

# ggexport(allPlotsBoutLength, filename = "rivBoutLengthModels.pdf", height = 15, width = 15)


########################################################################################
# N Howls in Bout
rivBinDataNbouts <- rivBinData %>% 
  filter(!is.na(avg_n_howls_in_bout))
########################################################################################

# null
nullNbouts <- lm(data = rivBinDataNbouts, formula = avg_n_howls_in_bout ~ 1, weights = n_obs)
nullNboutsAIC <- AIC(nullNbouts)
predData$nullNbouts <- predict(nullNbouts, newdata = predData)

nullNboutsplot <- ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Null Model (AIC = ", round(nullNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullNbouts))

# linear
linearNbouts <- lm(data = rivBinDataNbouts, formula = avg_n_howls_in_bout ~ avg_riv_dist, weights = n_obs)
linearNboutsAIC <- AIC(linearNbouts)
predData$linearNbouts <- predict(linearNbouts, newdata = predData)

linearNboutsplot <-ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Linear Model (AIC = ", round(linearNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearNbouts))


# power 
powerabcNbouts <- nlsLM(avg_n_howls_in_bout ~ a * ((avg_riv_dist)^b) + c, data = rivBinDataNbouts, 
                        start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
powerabcNboutsAIC <- AIC(powerabcNbouts)
predData$powerabcNbouts <- predict(powerabcNbouts, newdata = predData)

powerabcNboutsplot <-ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Power Model (AIC = ", round(powerabcNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcNbouts))




# logistic
logisticNbouts <- nlsLM(avg_n_howls_in_bout ~ a/(1+(b * exp(-c*(avg_riv_dist-0)/100))) + d, data = rivBinDataNbouts, 
                        start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
logisticNboutsAIC <- AIC(logisticNbouts)
predData$logisticNbouts <- predict(logisticNbouts, newdata = predData)

logisticNboutsplot <-ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Logistic Model (AIC = ", round(logisticNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticNbouts))


# segmented
segmentedNbouts <- segmented(linearNbouts, seg.Z = ~ avg_riv_dist, psi = 250)
segmentedNboutsAIC<- AIC(segmentedNbouts)
predData$segmentedNbouts <- predict(segmentedNbouts, newdata = predData)

segmentedNboutsplot <-ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Roars in Bout", 
       title = paste0("Segmented Model (AIC = ", round(segmentedNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNbouts))



# stepwise
stepwiseNbouts <- chngptm(
  formula.1 = avg_n_howls_in_bout ~ 1,
  formula.2 =  ~ avg_riv_dist,
  type = "step",
  family = "gaussian",
  data = rivBinDataNbouts
)
stepwiseNboutsAIC <- AIC(stepwiseNbouts)
predData$stepwiseNbouts <- predict(stepwiseNbouts, newdata = predData)

stepwiseNboutsplot <-ggplot(rivBinDataNbouts, aes(x = avg_riv_dist, y = avg_n_howls_in_bout)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_n_howls_in_bout - se_n_howls_in_bout, ymax = avg_n_howls_in_bout + se_n_howls_in_bout)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Mean Number of Roars in Bout", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNbouts <- ggarrange(nullNboutsplot, 
                            linearNboutsplot, 
                            powerabcNboutsplot, 
                            logisticNboutsplot, 
                            segmentedNboutsplot, 
                            stepwiseNboutsplot,
                            plottitleNbouts, ncol = 3, nrow = 3)

# ggexport(allPlotsNbouts, filename = "rivNroarsModels.pdf", height = 15, width = 15)




########################################################################################
# Howls per min
rivBinDataHowlMin <- rivBinData %>% 
  filter(!is.na(avg_howl_per_min))
########################################################################################

# null
nullHowlMin <- lm(data = rivBinDataHowlMin, formula = avg_howl_per_min ~ 1, weights = n_obs)
nullHowlMinAIC <- AIC(nullHowlMin)
predData$nullHowlMin <- predict(nullHowlMin, newdata = predData)

nullHowlMinplot <- ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Null Model (AIC = ", round(nullHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullHowlMin))

# linear
linearHowlMin <- lm(data = rivBinDataHowlMin, formula = avg_howl_per_min ~ avg_riv_dist, weights = n_obs)
linearHowlMinAIC <- AIC(linearHowlMin)
predData$linearHowlMin <- predict(linearHowlMin, newdata = predData)

linearHowlMinplot <-ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Linear Model (AIC = ", round(linearHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearHowlMin))


# power 
powerabcHowlMin <- nlsLM(avg_howl_per_min ~ a * ((avg_riv_dist)^b) + c, data = rivBinDataHowlMin, 
                        start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
powerabcHowlMinAIC <- AIC(powerabcHowlMin)
predData$powerabcHowlMin <- predict(powerabcHowlMin, newdata = predData)

powerabcHowlMinplot <-ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Power Model (AIC = ", round(powerabcHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcHowlMin))




# logistic
logisticHowlMin <- nlsLM(avg_howl_per_min ~ a/(1+(b * exp(-c*(avg_riv_dist-0)/100))) + d, data = rivBinDataHowlMin, 
                        start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
logisticHowlMinAIC <- AIC(logisticHowlMin)
predData$logisticHowlMin <- predict(logisticHowlMin, newdata = predData)

logisticHowlMinplot <-ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Logistic Model (AIC = ", round(logisticHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticHowlMin))


# segmented
segmentedHowlMin <- segmented(linearHowlMin, seg.Z = ~ avg_riv_dist, psi = 250)
segmentedHowlMinAIC<- AIC(segmentedHowlMin)
predData$segmentedHowlMin <- predict(segmentedHowlMin, newdata = predData)

segmentedHowlMinplot <-ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Segmented Model (AIC = ", round(segmentedHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedHowlMin))



# stepwise
stepwiseHowlMin <- chngptm(
  formula.1 = avg_howl_per_min ~ 1,
  formula.2 =  ~ avg_riv_dist,
  type = "step",
  family = "gaussian",
  data = rivBinDataHowlMin
)
stepwiseHowlMinAIC <- AIC(stepwiseHowlMin)
predData$stepwiseHowlMin <- predict(stepwiseHowlMin, newdata = predData)

stepwiseHowlMinplot <-ggplot(rivBinDataHowlMin, aes(x = avg_riv_dist, y = avg_howl_per_min)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_howl_per_min - se_howl_per_min, ymax = avg_howl_per_min + se_howl_per_min)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Howls per minute", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseHowlMinAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseHowlMin))


# plot title
plottitleHowlMin <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Mean Howls per minute", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsHowlMin <- ggarrange(nullHowlMinplot, 
                            linearHowlMinplot, 
                            powerabcHowlMinplot, 
                            logisticHowlMinplot, 
                            segmentedHowlMinplot, 
                            stepwiseHowlMinplot,
                            plottitleHowlMin, ncol = 3, nrow = 3)

# ggexport(allPlotsHowlMin, filename = "rivHowlMinModels.pdf", height = 15, width = 15)




########################################################################################
# N bouts per hour
riv_bin_data_n_bouts <- riv_bin_data_n_bouts %>% 
  filter(!is.na(avg_roar_bout_per_h))
########################################################################################

# null
null_n_bouts <- lm(data = riv_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ 1, weights = n_obs)
null_n_boutsAIC <- AIC(null_n_bouts)
pred_data_n_bouts$null_n_bouts <- predict(null_n_bouts, newdata = pred_data_n_bouts)

null_n_boutsplot <- ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Null Model (AIC = ", round(null_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = null_n_bouts))

# linear
linear_n_bouts <- lm(data = riv_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ avg_riv_dist, weights = n_obs)
linear_n_boutsAIC <- AIC(linear_n_bouts)
pred_data_n_bouts$linear_n_bouts <- predict(linear_n_bouts, newdata = pred_data_n_bouts)

linear_n_boutsplot <-ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Linear Model (AIC = ", round(linear_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = linear_n_bouts))


# power 
powerabc_n_bouts <- nlsLM(avg_roar_bout_per_h ~ a * ((avg_riv_dist)^b) + c, data = riv_bin_data_n_bouts, 
                          start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
powerabc_n_boutsAIC <- AIC(powerabc_n_bouts)
pred_data_n_bouts$powerabc_n_bouts <- predict(powerabc_n_bouts, newdata = pred_data_n_bouts)

powerabc_n_boutsplot <-ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Power Model (AIC = ", round(powerabc_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = powerabc_n_bouts))




# logistic
logistic_n_bouts <- nlsLM(avg_roar_bout_per_h ~ a/(1+(b * exp(-c*(avg_riv_dist-0)/100))) + d, data = riv_bin_data_n_bouts, 
                          start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
logistic_n_boutsAIC <- AIC(logistic_n_bouts)
pred_data_n_bouts$logistic_n_bouts <- predict(logistic_n_bouts, newdata = pred_data_n_bouts)

logistic_n_boutsplot <-ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Logistic Model (AIC = ", round(logistic_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = logistic_n_bouts))


# segmented
segmented_n_bouts <- segmented(linear_n_bouts, seg.Z = ~ avg_riv_dist, psi = 250)
segmented_n_boutsAIC<- AIC(segmented_n_bouts)
pred_data_n_bouts$segmented_n_bouts <- predict(segmented_n_bouts, newdata = pred_data_n_bouts)

segmented_n_boutsplot <-ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Number of Bouts per hour", 
       title = paste0("Segmented Model (AIC = ", round(segmented_n_boutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = segmented_n_bouts))



# stepwise
stepwise_n_bouts <- chngptm(
  formula.1 = avg_roar_bout_per_h ~ 1,
  formula.2 =  ~ avg_riv_dist,
  type = "step",
  family = "gaussian",
  data = riv_bin_data_n_bouts
)
stepwise_n_boutsAIC <- AIC(stepwise_n_bouts)
pred_data_n_bouts$stepwise_n_bouts <- predict(stepwise_n_bouts, newdata = pred_data_n_bouts)

stepwise_n_boutsplot <-ggplot(riv_bin_data_n_bouts, aes(x = avg_riv_dist, y = avg_roar_bout_per_h)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_bout_per_h - se_roar_bout_per_h, ymax = avg_roar_bout_per_h + se_roar_bout_per_h)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Mean Number of Bouts per hour", 
           hjust = 0.5, vjust = 0, size = 5)




allPlots_n_bouts <- ggarrange(null_n_boutsplot, 
                              linear_n_boutsplot, 
                              powerabc_n_boutsplot, 
                              logistic_n_boutsplot, 
                              segmented_n_boutsplot, 
                              stepwise_n_boutsplot,
                              plottitle_n_bouts, ncol = 3, nrow = 3)

# ggexport(allPlots_n_bouts, filename = "rivN_boutsModels.pdf", height = 15, width = 15)


########################################################################################
# Roaring Probability
riv_bin_data_howl_prob <- riv_bin_data_n_bouts %>% 
  filter(!is.na(avg_roar_prob))
########################################################################################

# null
null_howl_prob <- lm(data = riv_bin_data_howl_prob, formula = avg_roar_prob ~ 1, weights = n_obs)
null_howl_probAIC <- AIC(null_howl_prob)
pred_data_n_bouts$null_howl_prob <- predict(null_howl_prob, newdata = pred_data_n_bouts)

null_howl_probplot <- ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Null Model (AIC = ", round(null_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = null_howl_prob))

# linear
linear_howl_prob <- lm(data = riv_bin_data_howl_prob, formula = avg_roar_prob ~ avg_riv_dist, weights = n_obs)
linear_howl_probAIC <- AIC(linear_howl_prob)
pred_data_n_bouts$linear_howl_prob <- predict(linear_howl_prob, newdata = pred_data_n_bouts)

linear_howl_probplot <-ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Linear Model (AIC = ", round(linear_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = linear_howl_prob))


# power 
powerabc_howl_prob <- nlsLM(avg_roar_prob ~ a * ((avg_riv_dist)^b) + c, data = riv_bin_data_howl_prob, 
                          start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
powerabc_howl_probAIC <- AIC(powerabc_howl_prob)
pred_data_n_bouts$powerabc_howl_prob <- predict(powerabc_howl_prob, newdata = pred_data_n_bouts)

powerabc_howl_probplot <-ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Power Model (AIC = ", round(powerabc_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = powerabc_howl_prob))




# logistic
logistic_howl_prob <- nlsLM(avg_roar_prob ~ a/(1+(b * exp(-c*(avg_riv_dist-0)/100))) + d, data = riv_bin_data_howl_prob, 
                          start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
logistic_howl_probAIC <- AIC(logistic_howl_prob)
pred_data_n_bouts$logistic_howl_prob <- predict(logistic_howl_prob, newdata = pred_data_n_bouts)

logistic_howl_probplot <-ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Logistic Model (AIC = ", round(logistic_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = logistic_howl_prob))


# segmented
segmented_howl_prob <- segmented(linear_howl_prob, seg.Z = ~ avg_riv_dist, psi = 250)
segmented_howl_probAIC<- AIC(segmented_howl_prob)
pred_data_n_bouts$segmented_howl_prob <- predict(segmented_howl_prob, newdata = pred_data_n_bouts)

segmented_howl_probplot <-ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Probability of Roaring", 
       title = paste0("Segmented Model (AIC = ", round(segmented_howl_probAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = pred_data_n_bouts, aes(y = segmented_howl_prob))



# stepwise
stepwise_howl_prob <- chngptm(
  formula.1 = avg_roar_prob ~ 1,
  formula.2 =  ~ avg_riv_dist,
  type = "step",
  family = "gaussian",
  data = riv_bin_data_howl_prob
)
stepwise_howl_probAIC <- AIC(stepwise_howl_prob)
pred_data_n_bouts$stepwise_howl_prob <- predict(stepwise_howl_prob, newdata = pred_data_n_bouts)

stepwise_howl_probplot <-ggplot(riv_bin_data_howl_prob, aes(x = avg_riv_dist, y = avg_roar_prob)) +
  geom_point() + 
  geom_errorbar(aes(ymin = avg_roar_prob - se_roar_prob, ymax = avg_roar_prob + se_roar_prob)) +
  labs(x = "Distance from River Edge (m)", 
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
           label = "River Mean Probability of Roaring", 
           hjust = 0.5, vjust = 0, size = 5)




allPlots_howl_prob <- ggarrange(null_howl_probplot, 
                              linear_howl_probplot, 
                              powerabc_howl_probplot, 
                              logistic_howl_probplot, 
                              segmented_howl_probplot, 
                              stepwise_howl_probplot,
                              plottitle_howl_prob, ncol = 3, nrow = 3)

# ggexport(allPlots_howl_prob, filename = "rivhowl_probModels.pdf", height = 15, width = 15)
