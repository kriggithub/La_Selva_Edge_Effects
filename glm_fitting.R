#### 3/29/2026
#### Kurt Riggin
#### GLMM fitting



# Load packages
library(tidyverse)
library(emmeans)
library(multcomp)
library(ggpubr)



# Load data 
la_selva_data <- read.csv("all_la_selva_data.csv")


# Model Fitting 
# Factor edge type
la_selva_data$edge_type <- factor(la_selva_data$edge_type)
la_selva_data$edge_type <- relevel(la_selva_data$edge_type, ref = "I")



# Resting GLM
resting_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Rest_count = sum(Rest == 1, na.rm = T),
    notRest_count = sum(Rest == 0,  na.rm = T),
    .groups = "drop"
  )

# Full GLM
resting_glm <- glm(
  cbind(Rest_count, notRest_count) ~ edge_type,
  data = resting_grouped,
  family = binomial()
)

# Null GLM
resting_glm_null <- glm(
  cbind(Rest_count, notRest_count) ~ 1,
  data = resting_grouped,
  family = binomial()
)

# Likelihood ratio test (NOT SIGNIFICANT)
resting_lht <- anova(resting_glm_null, resting_glm, test = "Chisq")
resting_lht


resting_glm_est_ci <- emmeans(resting_glm, ~ edge_type, type = "response")
resting_glm_est_ci <- as.data.frame(resting_glm_est_ci)
resting_glm_est_ci$edge_type <- as.character(resting_glm_est_ci$edge_type)
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "I"] <- "Forest Interior"
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
resting_glm_est_ci$edge_type[resting_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
resting_glm_est_ci$.group <- "a"
resting_glm_est_ci

resting_label_height <- max(resting_glm_est_ci$asymp.UCL) * 1.005


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
  geom_text(aes(label = .group, y = resting_label_height), 
            size = 5, 
            fontface = "bold") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 11, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


resting_glm_plot

# ggsave(
#   plot = resting_glm_plot,
#   "resting_glm_plot.pdf",
#   width = 10,
#   height = 8,
#   dpi = 300
# )













# Feeding GLM
feeding_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Feed_count = sum(Feed == 1, na.rm = T),
    notFeed_count = sum(Feed == 0, na.rm = T),
    .groups = "drop"
  )

feeding_glm <- glm(
  cbind(Feed_count, notFeed_count) ~ edge_type,
  data = feeding_grouped,
  family = binomial()
)


feeding_glm_null <- glm(
  cbind(Feed_count, notFeed_count) ~ 1,
  data = feeding_grouped,
  family = binomial()
)

# Likelihood ratio test (SIGNIFICANT)
feeding_lht <- anova(feeding_glm_null, feeding_glm, test = "Chisq")
feeding_lht

# Generalized linear hypothesis test
feeding_glht <- glht(feeding_glm, linfct = mcp(edge_type = "Tukey")) # Tukey for all pairwise comparisons
summary(feeding_glht, test = adjusted("none"))








feeding_glm_est_ci <- emmeans(feeding_glm, ~ edge_type, type = "response")
feeding_letters <- cld(feeding_glm_est_ci, Letters = letters, adjust = "none")
feeding_glm_est_ci <- as.data.frame(feeding_letters)
feeding_glm_est_ci$edge_type <- as.character(feeding_glm_est_ci$edge_type)
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "I"] <- "Forest Interior"
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
feeding_glm_est_ci$edge_type[feeding_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
feeding_glm_est_ci$.group <- c("a","c","b")
feeding_glm_est_ci


feeding_label_height <- max(feeding_glm_est_ci$asymp.UCL) * 1.05



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
  geom_text(aes(label = .group, y = feeding_label_height), 
            size = 5, 
            fontface = "bold") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 11, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


feeding_glm_plot

# ggsave(
#   plot = feeding_glm_plot,
#   "feeding_glm_plot.pdf",
#   width = 10,
#   height = 8,
#   dpi = 300
# )








# Moving GLM
moving_grouped <- la_selva_data %>% 
  group_by(id, edge_type) %>% 
  summarise(
    Move_count = sum(Move == 1, na.rm = T),
    notMove_count = sum(Move == 0, na.rm = T),
    .groups = "drop"
  )

moving_glm <- glm(
  cbind(Move_count, notMove_count) ~ edge_type,
  data = moving_grouped,
  family = binomial()
)

moving_glm_null <- glm(
  cbind(Move_count, notMove_count) ~ 1,
  data = moving_grouped,
  family = binomial()
)

# Likelihood ratio test 
moving_lht <- anova(moving_glm_null, moving_glm, test = "Chisq")
moving_lht

# Generalized linear hypothesis test
moving_glht <- glht(moving_glm, linfct = mcp(edge_type = "Tukey")) # Tukey for all pairwise comparisons
summary(moving_glht, test = adjusted("none"))



moving_glm_est_ci <- emmeans(moving_glm, ~ edge_type, type = "response")
moving_letters <- cld(moving_glm_est_ci, Letters = letters, adjust = "none")
moving_glm_est_ci <- as.data.frame(moving_letters)
moving_glm_est_ci$edge_type <- as.character(moving_glm_est_ci$edge_type)
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "I"] <- "Forest Interior"
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
moving_glm_est_ci$edge_type[moving_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
moving_glm_est_ci$.group <- c("b", "b", "a")
moving_glm_est_ci


moving_label_height <- max(moving_glm_est_ci$asymp.UCL) * 1.05


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
  geom_text(aes(label = .group, y = moving_label_height), 
            size = 5, 
            fontface = "bold") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 11, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


moving_glm_plot

# ggsave(
#   plot = moving_glm_plot,
#   "moving_glm_plot.pdf",
#   width = 10,
#   height = 8,
#   dpi = 300
# )











# Num NN GLM

num_NN_glm <- glm(
  num_NN ~ edge_type,
  data = la_selva_data,
  family = poisson(link = "log")
)

num_NN_glm_null <- glm(
  num_NN ~ 1,
  data = la_selva_data,
  family = poisson(link = "log")
)


# Likelihood ratio test (Significant)
num_NN_lht <- anova(num_NN_glm_null, num_NN_glm, test = "Chisq")
num_NN_lht

num_NN_glht <- glht(num_NN_glm, linfct = mcp(edge_type = "Tukey"))
summary(num_NN_glht, test = adjusted("none"))


num_NN_glm_est_ci <- emmeans(num_NN_glm, ~ edge_type, type = "response")
num_NN_letters <- cld(num_NN_glm_est_ci, Letters = letters, adjust = "none")
print(num_NN_letters)


num_NN_glm_est_ci <- as.data.frame(num_NN_letters)
num_NN_glm_est_ci$edge_type <- as.character(num_NN_glm_est_ci$edge_type)
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "I"] <- "Forest Interior"
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "A"] <- "Anthropogenic Edge"
num_NN_glm_est_ci$edge_type[num_NN_glm_est_ci$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
num_NN_glm_est_ci$.group <- c("a","b","c")
num_NN_glm_est_ci

num_NN_label_height <- max(num_NN_glm_est_ci$asymp.UCL) * 1.015


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
  geom_text(aes(label = .group, y = num_NN_label_height), 
            size = 5, 
            fontface = "bold") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 11, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


num_NN_glm_plot

# ggsave(
#   plot = num_NN_glm_plot,
#   "num_NN_glm_plot.pdf",
#   width = 10,
#   height = 8,
#   dpi = 300
# )





# Dist NN GLM
la_selva_data <- la_selva_data %>%
  mutate(log_dist_NN = log(dist_NN + 0.1))


dist_NN_glm <- glm(
  log_dist_NN ~ edge_type,
  data = la_selva_data,
  family = gaussian()
)

dist_NN_glm_null <- glm(
  log_dist_NN ~ 1,
  data = la_selva_data,
  family = gaussian()
)

# Likelihood ratio test (Significant)
dist_NN_lht <- anova(dist_NN_glm_null, dist_NN_glm, test = "Chisq")
dist_NN_lht

dist_NN_glht <- glht(dist_NN_glm, linfct = mcp(edge_type = "Tukey"))
summary(dist_NN_glht, test = adjusted("none"))


dist_est <- emmeans(dist_NN_glm, ~ edge_type)
dist_NN_letters <- cld(dist_est, Letters = letters, adjust = "none")
print(dist_NN_letters)

dist_est <- as.data.frame(dist_NN_letters)

dist_est$mean_dist <- exp(dist_est$emmean) - 0.1
dist_est$lower <- exp(dist_est$lower.CL) - 0.1
dist_est$upper <- exp(dist_est$upper.CL) - 0.1

dist_est$edge_type <- as.character(dist_est$edge_type)
dist_est$edge_type[dist_est$edge_type == "I"] <- "Forest Interior"
dist_est$edge_type[dist_est$edge_type == "A"] <- "Anthropogenic Edge"
dist_est$edge_type[dist_est$edge_type == "B"] <- "Anthropogenic and Riparian Edge"
dist_est

dist_NN_label_height <- max(dist_est$upper) * 1.01


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
  geom_text(aes(label = .group, y = dist_NN_label_height), 
            size = 5, 
            fontface = "bold") +
  labs(
    x = "Forest Zone",
    y = "Mean Distance to Nearest Neighbor"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 11, color = "grey40"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

dist_NN_glm_plot


# ggsave(
#   plot = dist_NN_glm_plot,
#   "dist_NN_glm_plot.pdf",
#   width = 10,
#   height = 8,
#   dpi = 300
# )




# Final plots:

behavior_glm_plots <- ggarrange(resting_glm_plot,
                           feeding_glm_plot,
                           moving_glm_plot,
                             ncol = 2, nrow = 2,
                             labels   = c("a", "b", "c"),        # panel letters
                             label.x  = 0.01,                               # a little inset from left
                             label.y  = 0.98,                               # near the top
                             hjust    = 0,                                  # left aligned
                             vjust    = 1,                                  # top aligned
                             font.label = list(size = 20, face = "bold"))


ggexport(behavior_glm_plots, filename = "behavior_glm_plots.pdf", height = 12, width = 15.5)



cohesion_glm_plots <- ggarrange(num_NN_glm_plot,
                                dist_NN_glm_plot,
                                ncol = 2, nrow = 1,
                                labels   = c("a", "b"),        # panel letters
                                label.x  = 0.01,                               # a little inset from left
                                label.y  = 0.98,                               # near the top
                                hjust    = 0,                                  # left aligned
                                vjust    = 1,                                  # top aligned
                                font.label = list(size = 20, face = "bold"))


ggexport(cohesion_glm_plots, filename = "cohesion_glm_plots.pdf", height = 6, width = 15.5)









