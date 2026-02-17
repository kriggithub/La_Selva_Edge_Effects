#### 2/26/2026
#### Kurt Riggin
#### Data Cleaning


# Load packages
library(dplyr)
library(stringr)

# SET WD

# Load data
ls_2018 <- read.csv("La_Selva_2018_2019.csv", header = T)
ls_2022 <- read.csv("La_Selva_2022.csv", header = T)
ls_2023 <- read.csv("La_Selva_2023.csv", header = T)
ls_2024 <- read.csv("La_Selva_2024.csv", header = T)


# Combine data
la_selva_data <- bind_rows(ls_2018,ls_2022,ls_2023,ls_2024)

# Rename columns
la_selva_data <- la_selva_data %>% 
  rename(
    date = Date,
    id = GPS.waypoint,
    edge_type_original = Anth.Edge..River..Interior..Both.100m,
    activity = Activity,
    dist_NN = Dist.to.NN..m..with..10m,
    num_NN = X..NN.within.5.m,
    anth_dist = Anth.Distance,
    riv_dist = Raw.River.Distance,
  )

# Select columns
la_selva_data <- la_selva_data %>% 
  select(
    date,
    id, 
    edge_type_original, 
    activity,
    dist_NN,
    num_NN,
    lat,
    lon,
    anth_dist,
    riv_dist
         )

# Remove NA rows
la_selva_data <- la_selva_data %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(anth_dist))

# Capitalize IDs
la_selva_data$id <- str_to_upper(la_selva_data$id)

# Trim activity
la_selva_data$activity <- str_to_upper(str_trim(la_selva_data$activity))
la_selva_data$activity[la_selva_data$activity == "T"] <- "L"
la_selva_data$activity[la_selva_data$activity == "M"] <- "L"
la_selva_data$activity[la_selva_data$activity == "OV" | la_selva_data$activity == "" | la_selva_data$activity == "ACTIVITY"] <- NA
unique(la_selva_data$activity)

# Update Distance to nearest neighbors
la_selva_data$dist_NN[la_selva_data$dist_NN == "" | la_selva_data$dist_NN == ">10"] <- NA

unique(la_selva_data$dist_NN)




# Create column for edge type
la_selva_data <- la_selva_data %>% 
  mutate(
    edge_type = case_when(
      anth_dist < 100 & riv_dist > 100 ~ "A",
      riv_dist < 100 & anth_dist > 100 ~ "R",
      anth_dist < 100 & riv_dist < 100 ~ "B",
      anth_dist > 100 & riv_dist > 100 ~ "I",
      TRUE ~ "unknown"
    )
  ) %>% 
  select(-edge_type_original)

# Create count columns
la_selva_data$Rest <- as.numeric(la_selva_data$activity =="R")
la_selva_data$Feed <- as.numeric(la_selva_data$activity =="F")
la_selva_data$Move <- as.numeric(la_selva_data$activity =="L")
la_selva_data$Social <- as.numeric(la_selva_data$activity =="S")



# Export combined data
# write.csv(la_selva_data, file = "all_la_selva_data.csv")







