### Kurt Riggin
### 4/2/2026
### Data cleaning script


# Load packages
library(tidyverse)

# Load in data
length_number_data <- read.csv("10_min_roar_bout_length_#_per_bout.csv")


# Rename variables
length_number_data <- length_number_data %>% 
  rename(
    sample_id = GPS.waypoint.for.howl,
    anth_dist = Anth.Distance..Perimeter...Camp.,
    riv_dist = Raw.River.Distance,
    bout_length_s = Length.of.howl.bout.in.seconds,
    n_howls_in_bout = Howl.number.in.bout
  ) %>% 
  select(
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
