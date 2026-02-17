#### 2/26/2026
#### Kurt Riggin
#### GLMM fitting



# Load packages
library(glmmTMB)
library(tidyverse)
library(emmeans)

# Load data 
la_selva_data <- read.csv("all_la_selva_data.csv")


# Model Fitting (Forest zone fixed effect, ID random effect, and null models)
# Factor edge type
la_selva_data$edge_type <- factor(la_selva_data$edge_type)
la_selva_data$edge_type <- relevel(la_selva_data$edge_type, ref = "I")

# Resting
resting_model <- glmmTMB(
  Rest ~ edge_type + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(resting_model)

resting_null_model <- glmmTMB(
  Rest ~ 1 + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(resting_null_model)

rest_est_ci <- emmeans(resting_model, ~ edge_type, type = "response")
rest_est_ci


# Feeding
feeding_model <- glmmTMB(
  Feed ~ edge_type + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(feeding_model)

feeding_null_model <- glmmTMB(
  Feed ~ 1 + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(feeding_null_model)


feed_est_ci <- emmeans(feeding_model, ~ edge_type, type = "response")
feed_est_ci




# Moving
moving_model <- glmmTMB(
  Move ~ edge_type + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(moving_model)

moving_null_model <- glmmTMB(
  Move ~ 1 + (1|id), 
  data = la_selva_data, 
  family = binomial(link = "logit")
  )
summary(moving_null_model)

move_est_ci <- emmeans(moving_model, ~ edge_type, type = "response")
move_est_ci



# Number of nearest neighbors (SIGNIFICANT)
la_selva_data$num_NN <- as.numeric(la_selva_data$num_NN)

num_NN_model <- glmmTMB(
  num_NN ~ edge_type + (1|id), 
  data = la_selva_data, 
  family = poisson(link = "log")
  )
summary(num_NN_model)

num_NN_null_model <- glmmTMB(
  num_NN ~ 1 + (1|id), 
  data = la_selva_data, 
  family = poisson(link = "log")
  )
summary(num_NN_null_model)

num_NN_est_ci <- emmeans(num_NN_model, ~ edge_type, type = "response")
num_NN_est_ci


# Distance to nearest neighbors
hist(la_selva_data$dist_NN)
la_selva_data <- la_selva_data %>%
  mutate(log_dist_NN = log(dist_NN + 0.1))
hist(la_selva_data$log_dist_NN)

dist_NN_model <- glmmTMB(
  log_dist_NN ~ edge_type + (1 | id),
  data = la_selva_data,
  family = gaussian()
)
summary(dist_NN_model)

dist_NN_null_model <- glmmTMB(
  log_dist_NN ~ 1 + (1 | id),
  data = la_selva_data,
  family = gaussian()
)
summary(dist_NN_null_model)

dist_NN_est_ci <- emmeans(dist_NN_model, ~ edge_type)
dist_NN_est_ci

dist_df <- as.data.frame(dist_NN_est_ci)

dist_df <- dist_df %>%
  mutate(
    mean_dist = exp(emmean) - 0.1,
    lower = exp(asymp.LCL) - 0.1,
    upper = exp(asymp.UCL) - 0.1
  )

dist_df



