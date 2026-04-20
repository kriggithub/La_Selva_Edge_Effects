### Kurt Riggin
### 4/2/2026
### Data cleaning script


# Load packages
library(tidyverse)

# Load in data
length_number_data <- read.csv("10_min_roar_bout_length_#_per_bout.csv")
bout_h_data <- read.csv("10_min_roar_bouts_per_hours.csv")


# Rename variables (Dataset 1)
length_number_data <- length_number_data %>% 
  rename(
    sample_id = GPS.waypoint.for.howl,
    anth_dist = Anth.Distance..Perimeter...Camp.,
    riv_dist = Raw.River.Distance,
    bout_length_s = Length.of.howl.bout.in.seconds,
    n_howls_in_bout = Howl.number.in.bout
  ) %>%
  dplyr::select(
    sample_id,
    anth_dist,
    riv_dist,
    bout_length_s,
    n_howls_in_bout
  ) %>%
  mutate(
    howl_per_min = ((n_howls_in_bout)/(bout_length_s))*60
  ) %>%
  drop_na()



anth_length_number_data <- length_number_data %>% 
  mutate(bin = as.factor(ceiling(anth_dist / 15))) %>%
  group_by(bin) %>% 
  dplyr::summarise(
    n_obs = n(),
    avg_bout_length_s = mean(bout_length_s, na.rm = T),
    sd_bout_length_s = sd(bout_length_s, na.rm = T),
    se_bout_length_s = sd_bout_length_s / sqrt(n_obs),
    avg_n_howls_in_bout = mean(n_howls_in_bout, na.rm = T),
    sd_n_howls_in_bout = sd(n_howls_in_bout, na.rm = T),
    se_n_howls_in_bout = sd_n_howls_in_bout / sqrt(n_obs),
    avg_howl_per_min = mean(howl_per_min, na.rm = T),
    sd_howl_per_min = sd(howl_per_min, na.rm = T),
    se_howl_per_min = sd_howl_per_min / sqrt(n_obs),
    avg_anth_dist = mean(anth_dist, na.rm = T)
  ) %>% 
  ungroup()


riv_length_number_data <- length_number_data %>% 
  mutate(bin = as.factor(ceiling(riv_dist / 15))) %>%
  group_by(bin) %>% 
  dplyr::summarise(
    n_obs = n(),
    avg_bout_length_s = mean(bout_length_s, na.rm = T),
    sd_bout_length_s = sd(bout_length_s, na.rm = T),
    se_bout_length_s = sd_bout_length_s / sqrt(n_obs),
    avg_n_howls_in_bout = mean(n_howls_in_bout, na.rm = T),
    sd_n_howls_in_bout = sd(n_howls_in_bout, na.rm = T),
    se_n_howls_in_bout = sd_n_howls_in_bout / sqrt(n_obs),
    avg_howl_per_min = mean(howl_per_min, na.rm = T),
    sd_howl_per_min = sd(howl_per_min, na.rm = T),
    se_howl_per_min = sd_howl_per_min / sqrt(n_obs),
    avg_riv_dist = mean(riv_dist, na.rm = T)
  ) %>% 
  ungroup()


# write.csv(anth_length_number_data, file = "anth_n_howls_in_bout.csv")
# write.csv(riv_length_number_data, file = "riv_n_howls_in_bout.csv")








bout_h_data <- bout_h_data %>% 
  rename(
    sample_id = WAYPOINT,
    anth_dist = Anth.Distance..m.,
    riv_dist = Raw.River.Distance..m.,
    roar_bout_per_h = roar.bouts.per.hour
  ) %>% 
  dplyr::select(
    sample_id,
    anth_dist,
    riv_dist,
    roar_bout_per_h
  ) %>% 
  drop_na() %>% 
  mutate(
    roar_occur = ifelse(roar_bout_per_h > 0, 1, 0)
  )




anth_bout_h_data <- bout_h_data %>% 
  mutate(bin = as.factor(ceiling(anth_dist / 15))) %>%
  group_by(bin) %>% 
  dplyr::summarise(
    n_obs = n(),
    avg_roar_bout_per_h = mean(roar_bout_per_h, na.rm = T),
    sd_roar_bout_per_h = sd(roar_bout_per_h, na.rm = T),
    se_roar_bout_per_h = sd_roar_bout_per_h / sqrt(n_obs),
    avg_roar_prob = mean(roar_occur, na.rm = T),
    sd_roar_prob = sd(roar_occur, na.rm = T),
    se_roar_prob = sd_roar_prob / sqrt(n_obs),
    avg_anth_dist = mean(anth_dist, na.rm = T)
  ) %>% 
  ungroup()



riv_bout_h_data <- bout_h_data %>% 
  mutate(bin = as.factor(ceiling(riv_dist / 15))) %>%
  group_by(bin) %>% 
  dplyr::summarise(
    n_obs = n(),
    avg_roar_bout_per_h = mean(roar_bout_per_h, na.rm = T),
    sd_roar_bout_per_h = sd(roar_bout_per_h, na.rm = T),
    se_roar_bout_per_h = sd_roar_bout_per_h / sqrt(n_obs),
    avg_roar_prob = mean(roar_occur, na.rm = T),
    sd_roar_prob = sd(roar_occur, na.rm = T),
    se_roar_prob = sd_roar_prob / sqrt(n_obs),
    avg_riv_dist = mean(riv_dist, na.rm = T)
  ) %>% 
  ungroup()


# write.csv(anth_bout_h_data, file = "anth_roar_bout_per_h.csv")
# write.csv(riv_bout_h_data, file = "riv_roar_bout_per_h.csv")



