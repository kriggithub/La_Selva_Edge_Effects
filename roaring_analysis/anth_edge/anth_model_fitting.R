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
# N Howls in Bout
anthBinDataNbouts <- anthBinData %>% 
  filter(!is.na(avg_n_howls_in_bout))
########################################################################################

# null
nullNbouts <- lm(data = anthBinDataNbouts, formula = avg_n_howls_in_bout ~ 1, weights = n_obs)
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
linearNbouts <- lm(data = anthBinDataNbouts, formula = avg_n_howls_in_bout ~ avg_anth_dist, weights = n_obs)
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
                         start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
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
                         start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
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








########################################################################################
# N bouts per hour
anth_bin_data_n_bouts <- anth_bin_data_n_bouts %>% 
  filter(!is.na(avg_roar_bout_per_h))
########################################################################################

# null
null_n_bouts <- lm(data = anth_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ 1, weights = n_obs)
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
linear_n_bouts <- lm(data = anth_bin_data_n_bouts, formula = avg_roar_bout_per_h ~ avg_anth_dist, weights = n_obs)
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
                        start = list(a = 1, b = 1, c = 80), weights = n_obs, control = nls.lm.control(maxiter = 1000))
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
                        start = list(a = 90, b = 20, c = 2.5, d = 40), weights = n_obs, control = nls.lm.control(maxiter = 1000))
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

