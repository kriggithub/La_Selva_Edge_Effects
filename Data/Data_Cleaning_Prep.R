#### 2/26/2026
#### Kurt Riggin
#### Data Cleaning


# Load packages
library(dplyr)
library(stringr)
library(Hmisc)

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
  dplyr::select(
    date,
    id, 
    edge_type_original, 
    activity,
    dist_NN,
    num_NN,
    lat,
    lon,
    anth_dist,
    riv_dist)

# Remove NA rows
la_selva_data <- la_selva_data %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(anth_dist))

# Capitalize IDs
la_selva_data$id <- str_to_upper(la_selva_data$id)

# Trim activity
la_selva_data$activity <- str_to_upper(str_trim(la_selva_data$activity))

la_selva_data$activity[la_selva_data$activity == "T"] <- "L"
la_selva_data$activity[la_selva_data$activity == "OV" | la_selva_data$activity == "" | la_selva_data$activity == "ACTIVITY"] <- NA



unique(la_selva_data$activity)
table(la_selva_data$activity)

# Update Distance to nearest neighbors ()
la_selva_data$dist_NN[la_selva_data$dist_NN == ""] <- NA
la_selva_data$dist_NN[la_selva_data$dist_NN == ">10"] <- 13.4

unique(la_selva_data$dist_NN)

table(la_selva_data$dist_NN)


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
  dplyr::select(-edge_type_original)

# Create count columns
la_selva_data$Rest <- as.numeric(la_selva_data$activity =="R")
la_selva_data$Feed <- as.numeric(la_selva_data$activity =="F")
la_selva_data$Move <- as.numeric(la_selva_data$activity =="L")
la_selva_data$Social <- as.numeric(la_selva_data$activity =="S")
la_selva_data$dist_NN <- as.numeric(la_selva_data$dist_NN)



# Export combined data
# write.csv(la_selva_data, file = "all_la_selva_data.csv")






### Data Frame 1: Summarized by ID (monkey)

monkeyIdData <- la_selva_data %>% 
  group_by(id) %>% 
  dplyr::summarize(RestPct = mean(activity == "R")*100,
                   MovingPct = mean(activity == "L")*100,
                   FeedingPct = mean(activity == "F")*100,
                   OtherPct = mean(!(activity %in% c("R", "L", "F")))*100,
                   AvgNumNN = mean(num_NN, na.rm = T),
                   AvgDistNN = mean(dist_NN, na.rm = T),
                   RivDist = mean(riv_dist),
                   AnthDist = mean(anth_dist),
                   nObs = n())
write.csv(monkeyIdData, file = "monkeyIdData.csv")



### Data Frame 2: Summarized within band (Anth)

anthBinData <- la_selva_data %>% 
  mutate(bin = as.factor(ceiling(anth_dist / 15))) %>% 
  group_by(id) %>% 
  mutate(
    RestPct = mean(activity == "R", na.rm = TRUE) * 100,
    MovingPct = mean(activity == "L", na.rm = TRUE) * 100,
    FeedingPct = mean(activity == "F", na.rm = TRUE) * 100,
    OtherPct = mean(!(activity %in% c("R", "L", "F")), na.rm = TRUE) * 100,
    AvgNumNN = mean(num_NN, na.rm = TRUE),
    AvgDistNN = mean(dist_NN, na.rm = TRUE),
    RivDist = mean(riv_dist, na.rm = TRUE),
    AnthDist = mean(anth_dist, na.rm = TRUE),
    nObs = n()
  ) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  group_by(bin) %>% 
  dplyr::summarize(nMonkeys = n(),
                   wtAvgRestPct = weighted.mean(RestPct, nObs, na.rm = T),
                   wtSdRestPct = sqrt(wtd.var(RestPct, nObs, na.rm = T)),
                   wtSeRestPct = wtSdRestPct/sqrt(nMonkeys),
                   wtAvgMovingPct = weighted.mean(MovingPct, nObs, na.rm = T),
                   wtSdMovingPct = sqrt(wtd.var(MovingPct, nObs, na.rm = T)),
                   wtSeMovingPct = wtSdMovingPct/sqrt(nMonkeys),
                   wtAvgFeedingPct = weighted.mean(FeedingPct, nObs, na.rm = T),
                   wtSdFeedingPct = sqrt(wtd.var(FeedingPct, nObs, na.rm = T)),
                   wtSeFeedingPct = wtSdFeedingPct/sqrt(nMonkeys),
                   wtAvgOtherPct = weighted.mean(OtherPct, nObs, na.rm = T),
                   wtSdOtherPct = sqrt(wtd.var(OtherPct, nObs, na.rm = T)),
                   wtSeOtherPct = wtSdOtherPct/sqrt(nMonkeys),
                   wtAvgNumNN = weighted.mean(AvgNumNN, nObs, na.rm = T),
                   wtSdNumNN = sqrt(wtd.var(AvgNumNN, nObs, na.rm = T)),
                   wtSeNumNN = wtSdNumNN/sqrt(nMonkeys),
                   wtAvgDistNN = weighted.mean(AvgDistNN, nObs, na.rm = T),
                   wtSdDistNN = sqrt(wtd.var(AvgDistNN, nObs, na.rm = T)),
                   wtSeDistNN = wtSdDistNN/sqrt(nMonkeys),
                   wtAvgAnthDist = weighted.mean(AnthDist, nObs, na.rm = T),
                   wtSdAnthDist = sqrt(wtd.var(AnthDist, nObs, na.rm = T)),
                   wtSeAnthDist = wtSdAnthDist/sqrt(nMonkeys))


# SE = Sd / sqrt (n Monkeys in bin)



# write.csv(anthBinData, file = "anthBinData.csv")





### Data Frame 3: Summarized within band (Riv)

rivBinData <- la_selva_data %>% 
  mutate(bin = as.factor(ceiling(riv_dist / 15))) %>% 
  group_by(id) %>% 
  mutate(
    RestPct = mean(activity == "R", na.rm = TRUE) * 100,
    MovingPct = mean(activity == "L", na.rm = TRUE) * 100,
    FeedingPct = mean(activity == "F", na.rm = TRUE) * 100,
    OtherPct = mean(!(activity %in% c("R", "L", "F")), na.rm = TRUE) * 100,
    AvgNumNN = mean(num_NN, na.rm = TRUE),
    AvgDistNN = mean(dist_NN, na.rm = TRUE),
    RivDist = mean(riv_dist, na.rm = TRUE),
    AnthDist = mean(anth_dist, na.rm = TRUE),
    nObs = n()
  ) %>% 
  filter(row_number()==1) %>%
  ungroup() %>% 
  group_by(bin) %>% 
  dplyr::summarize(nMonkeys = n(),
                   wtAvgRestPct = weighted.mean(RestPct, nObs, na.rm = T),
                   wtSdRestPct = sqrt(wtd.var(RestPct, nObs, na.rm = T)),
                   wtSeRestPct = wtSdRestPct/sqrt(nMonkeys),
                   wtAvgMovingPct = weighted.mean(MovingPct, nObs, na.rm = T),
                   wtSdMovingPct = sqrt(wtd.var(MovingPct, nObs, na.rm = T)),
                   wtSeMovingPct = wtSdMovingPct/sqrt(nMonkeys),
                   wtAvgFeedingPct = weighted.mean(FeedingPct, nObs, na.rm = T),
                   wtSdFeedingPct = sqrt(wtd.var(FeedingPct, nObs, na.rm = T)),
                   wtSeFeedingPct = wtSdFeedingPct/sqrt(nMonkeys),
                   wtAvgOtherPct = weighted.mean(OtherPct, nObs, na.rm = T),
                   wtSdOtherPct = sqrt(wtd.var(OtherPct, nObs, na.rm = T)),
                   wtSeOtherPct = wtSdOtherPct/sqrt(nMonkeys),
                   wtAvgNumNN = weighted.mean(AvgNumNN, nObs, na.rm = T),
                   wtSdNumNN = sqrt(wtd.var(AvgNumNN, nObs, na.rm = T)),
                   wtSeNumNN = wtSdNumNN/sqrt(nMonkeys),
                   wtAvgDistNN = weighted.mean(AvgDistNN, nObs, na.rm = T),
                   wtSdDistNN = sqrt(wtd.var(AvgDistNN, nObs, na.rm = T)),
                   wtSeDistNN = wtSdDistNN/sqrt(nMonkeys),
                   wtAvgRivDist = weighted.mean(RivDist, nObs, na.rm = T),
                   wtSdRivDist = sqrt(wtd.var(RivDist, nObs, na.rm = T)),
                   wtSeRivDist = wtSdRivDist/sqrt(nMonkeys))


# write.csv(rivBinData, file = "rivBinData.csv")


# save.image("DataCleaning.RData")





