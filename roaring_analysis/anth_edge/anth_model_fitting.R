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


# create prediction dataframe

predData <- data.frame(
  avg_anth_dist = seq(min(anthBinData$avg_anth_dist, na.rm = T),
                      max(anthBinData$avg_anth_dist, na.rm = T),
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
       y = "(Weighted) Mean Number of Bouts", 
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
       y = "(Weighted) Mean Number of Bouts", 
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
       y = "(Weighted) Mean Number of Bouts", 
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
       y = "(Weighted) Mean Number of Bouts", 
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
       y = "(Weighted) Mean Number of Bouts", 
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
       y = "(Weighted) Mean Number of Bouts", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseNboutsAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseNbouts))


# plot title
plottitleNbouts <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Anthropogenic Mean Number of Bouts", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNbouts <- ggarrange(nullNboutsplot, 
                             linearNboutsplot, 
                             powerabcNboutsplot, 
                             logisticNboutsplot, 
                             segmentedNboutsplot, 
                             stepwiseNboutsplot,
                             plottitleNbouts, ncol = 3, nrow = 3)

#ggexport(allPlotsNbouts, filename = "anthNboutsModels.pdf", height = 15, width = 15)

