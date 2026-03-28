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



# Resting GLMM
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
rest_est_ci <- as.data.frame(rest_est_ci)
rest_est_ci$edge_type <- as.character(rest_est_ci$edge_type)
rest_est_ci$edge_type[rest_est_ci$edge_type == "I"] <- "Forest Interior"
rest_est_ci$edge_type[rest_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
rest_est_ci$edge_type[rest_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
rest_est_ci

prob_rest_plot <- ggplot(rest_est_ci, 
                         aes(x = factor(edge_type, levels = c("Forest Interior",
                                                              "Anthropogenic Edge",
                                                              "Anthropogenic and Riparian Edge")), 
                             y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Resting",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


prob_rest_plot

ggsave(
  plot = prob_rest_plot,
  "prob_rest_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)


# Resting GLM Grouped
resting_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Rest = sum(Rest == 1),
    notRest = sum(Rest == 0),
    .groups = "drop"
  )

resting_glm <- glm(
  cbind(Rest, notRest) ~ edge_type,
  data = resting_grouped,
  family = binomial()
)



resting_glm_est_ci <- emmeans(resting_glm, ~ edge_type, type = "response")
resting_glm_est_ci <- as.data.frame(resting_glm_est_ci)
resting_glm_est_ci$edge_type <- as.character(resting_glm_est_ci$edge_type)
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "I"] <- "Forest Interior"
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
resting_glm_est_ci



resting_glm_plot <- ggplot(resting_glm_est_ci, 
                                 aes(x = factor(edge_type, levels = c("Forest Interior",
                                                                      "Anthropogenic Edge",
                                                                      "Anthropogenic and Riparian Edge")), 
                                     y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Resting",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


resting_glm_plot

ggsave(
  plot = resting_glm_plot,
  "resting_glm_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)













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
feed_est_ci <- as.data.frame(feed_est_ci)
feed_est_ci$edge_type <- as.character(feed_est_ci$edge_type)
feed_est_ci$edge_type[feed_est_ci$edge_type == "I"] <- "Forest Interior"
feed_est_ci$edge_type[feed_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
feed_est_ci$edge_type[feed_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
feed_est_ci

prob_feed_plot <- ggplot(feed_est_ci, 
                         aes(x = factor(edge_type, levels = c("Forest Interior",
                                                              "Anthropogenic Edge",
                                                              "Anthropogenic and Riparian Edge")), 
                             y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Feeding",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


prob_feed_plot

ggsave(
  plot = prob_feed_plot,
  "prob_feed_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)

# Feeding GLM Grouped
feeding_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Feed = sum(Feed == 1),
    notFeed = sum(Feed == 0),
    .groups = "drop"
  )

feeding_glm <- glm(
  cbind(Feed, notFeed) ~ edge_type,
  data = feeding_grouped,
  family = binomial()
)



feeding_glm_est_ci <- emmeans(feeding_glm, ~ edge_type, type = "response")
feeding_glm_est_ci <- as.data.frame(feeding_glm_est_ci)
feeding_glm_est_ci$edge_type <- as.character(feeding_glm_est_ci$edge_type)
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "I"] <- "Forest Interior"
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
feeding_glm_est_ci



feeding_glm_plot <- ggplot(feeding_glm_est_ci, 
                           aes(x = factor(edge_type, levels = c("Forest Interior",
                                                                "Anthropogenic Edge",
                                                                "Anthropogenic and Riparian Edge")), 
                               y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Feeding",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


feeding_glm_plot

ggsave(
  plot = feeding_glm_plot,
  "feeding_glm_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)







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
move_est_ci <- as.data.frame(move_est_ci)
move_est_ci$edge_type <- as.character(move_est_ci$edge_type)
move_est_ci$edge_type[move_est_ci$edge_type == "I"] <- "Forest Interior"
move_est_ci$edge_type[move_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
move_est_ci$edge_type[move_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
move_est_ci

prob_move_plot <- ggplot(move_est_ci, 
                         aes(x = factor(edge_type, levels = c("Forest Interior",
                                                              "Anthropogenic Edge",
                                                              "Anthropogenic and Riparian Edge")), 
                             y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Moving",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


prob_move_plot

ggsave(
  plot = prob_move_plot,
  "prob_move_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)



# Moving GLM Grouped
moving_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Move = sum(Move == 1),
    notMove = sum(Move == 0),
    .groups = "drop"
  )

moving_glm <- glm(
  cbind(Move, notMove) ~ edge_type,
  data = moving_grouped,
  family = binomial()
)



moving_glm_est_ci <- emmeans(moving_glm, ~ edge_type, type = "response")
moving_glm_est_ci <- as.data.frame(moving_glm_est_ci)
moving_glm_est_ci$edge_type <- as.character(moving_glm_est_ci$edge_type)
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "I"] <- "Forest Interior"
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
moving_glm_est_ci



moving_glm_plot <- ggplot(moving_glm_est_ci, 
                           aes(x = factor(edge_type, levels = c("Forest Interior",
                                                                "Anthropogenic Edge",
                                                                "Anthropogenic and Riparian Edge")), 
                               y = prob)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Probability of Moving",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


moving_glm_plot

ggsave(
  plot = moving_glm_plot,
  "moving_glm_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)












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
num_NN_est_ci <- as.data.frame(num_NN_est_ci)
num_NN_est_ci$edge_type <- as.character(num_NN_est_ci$edge_type)
num_NN_est_ci$edge_type[num_NN_est_ci$edge_type == "I"] <- "Forest Interior"
num_NN_est_ci$edge_type[num_NN_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
num_NN_est_ci$edge_type[num_NN_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
num_NN_est_ci

mean_num_NN_plot <- ggplot(num_NN_est_ci, 
                         aes(x = factor(edge_type, levels = c("Forest Interior",
                                                              "Anthropogenic Edge",
                                                              "Anthropogenic and Riparian Edge")), 
                             y = rate)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Mean Number of Nearest Neighbors within 5 m",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


mean_num_NN_plot

ggsave(
  plot = mean_num_NN_plot,
  "mean_num_NN_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)



# Num NN GLM 


num_NN_glm <- glm(
  num_NN ~ edge_type,
  data = la_selva_data,
  family = poisson(link = "log")
)



num_NN_glm_est_ci <- emmeans(num_NN_glm, ~ edge_type, type = "response")
num_NN_glm_est_ci <- as.data.frame(num_NN_glm_est_ci)
num_NN_glm_est_ci$edge_type <- as.character(num_NN_glm_est_ci$edge_type)
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "I"] <- "Forest Interior"
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
num_NN_glm_est_ci



num_NN_glm_plot <- ggplot(num_NN_glm_est_ci, 
                          aes(x = factor(edge_type, levels = c("Forest Interior",
                                                               "Anthropogenic Edge",
                                                               "Anthropogenic and Riparian Edge")), 
                              y = rate)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, 
                    ymax = asymp.UCL),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Mean Number of Nearest Neighbors within 5 m",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


num_NN_glm_plot

ggsave(
  plot = num_NN_glm_plot,
  "num_NN_glm_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)









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



dist_df$edge_type <- as.character(dist_df$edge_type)
dist_df$edge_type[dist_df$edge_type == "I"] <- "Forest Interior"
dist_df$edge_type[dist_df$edge_type == "A"] <- "Anthropogenic Edge"
dist_df$edge_type[dist_df$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
dist_df

mean_dist_NN_plot <- ggplot(dist_df, 
                           aes(x = factor(edge_type, levels = c("Forest Interior",
                                                                "Anthropogenic Edge",
                                                                "Anthropogenic and Riparian Edge")), 
                               y = mean_dist)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper),
                width = 0, 
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Mean Distance to Nearest Neighbor (m)",
  ) + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


mean_dist_NN_plot

ggsave(
  plot = mean_dist_NN_plot,
  "mean_dist_NN_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)



# Dist NN GLM
la_selva_data <- la_selva_data %>%
  mutate(log_dist_NN = log(dist_NN + 0.1))


dist_NN_glm <- glm(
  log_dist_NN ~ edge_type,
  data = la_selva_data,
  family = gaussian()
)


dist_est <- emmeans(dist_NN_glm, ~ edge_type)
dist_est <- as.data.frame(dist_est)

dist_est$mean_dist <- exp(dist_est$emmean) - 0.1
dist_est$lower <- exp(dist_est$lower.CL) - 0.1
dist_est$upper <- exp(dist_est$upper.CL) - 0.1

dist_est$edge_type <- as.character(dist_est$edge_type)
dist_est$edge_type[dist_est$edge_type == "I"] <- "Forest Interior"
dist_est$edge_type[dist_est$edge_type == "A"] <- "Anthropogenic Edge"
dist_est$edge_type[dist_est$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
dist_est


dist_NN_glm_plot <- ggplot(
  dist_est,
  aes(
    x = factor(edge_type, levels = c(
      "Forest Interior",
      "Anthropogenic Edge",
      "Anthropogenic and Riparian Edge"
    )),
    y = mean_dist
  )
) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0,
                linewidth = 1) +
  labs(
    x = "Forest Zone",
    y = "Mean Distance to Nearest Neighbor"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

dist_NN_glm_plot


ggsave(
  plot = dist_NN_glm_plot,
  "dist_NN_glm_plot.pdf",
  width = 10,
  height = 8,
  dpi = 300
)



