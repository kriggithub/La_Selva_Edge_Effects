library(knitr)
library(dplyr)
library(gt)
library(webshot2)

results <- tribble(
  ~Response, ~Null, ~Linear, ~Power,  ~Logistic,  ~Segmented, ~Changepoint, ~PseudoR2,    ~DEI,
  "(A) Distance from Nearest Neighbors", 76.73, 75.98,   77.65, 74.95, 77.52, 88.65, 0.24, "127 (102-153)",
  "(A) Number of Nearest Neighbors", 77.33, 78.16,   78.94, 75.14, 75.03, 93.28, 0.25, "113 (39-186)",
  "(A) % Time Feeding", 212.03, 211.00,   212.33, 211.86, 212.92, 221.62, 0.09, "416 (21-601)",
  "(A) % Time Moving", 197.26, 196.93,   197.39, 196.29, 193.5, 225.59, 0.27, "53 (-52-157)",
  "(A) % Time Resting", 235.16, 236.89,   236.65, 239.49, 236.52, 250.87, 0.00,  "No Edge Effect",
  "(R) Distance from Nearest Neighbors", 107.07, 106.25,   108.19, 110.23, 109.74, 150.84, 0.08,  "455 (30-658)",
  "(R) Number of Nearest Neighbors", 93.22, 95.17,   95.83, 95.04, 87.93, 116.80, 0.28, "157 (53-261)",
  "(R) % Time Feeding", 267.18, 268.93,   269.73, 271.86, 272.1, 279.05, 0.00, "No Edge Effect",
  "(R) % Time Moving", 236.55, 235.87,   237.36, 236.94, 235.19, 260.5, 0.18,  "66 (21-112)",
  "(R) % Time Resting", 289.31, 291.24,   292.95, 294.71, 292.30, 301.64, 0.00,  "No Edge Effect"
)


results_clean <- results %>%
  mutate(
    Edge = ifelse(grepl("^\\(A\\)", Response), 
                  "Anthropogenic Edge", 
                  "Riparian Edge"),
    Response = gsub("^\\(A\\) |^\\(R\\) ", "", Response)
  )





# Clean up response column and create grouping variable
table <- results_clean %>%
  gt(rowname_col = "Response", groupname_col = "Edge") %>%
  tab_stubhead(label = "Response") %>%
  # tab_header(
  #   title = "All Edge Model Fitting",
  #   subtitle = "AIC and Pseudo-R² Comparison"
  # ) %>%
  cols_label(
    PseudoR2 = html("Pseudo-R² for best model"),
    DEI = html("Mean depth of edge influence (DEI) (95% CI)")# rename column
  ) %>%
  # --- Anthropogenic edge bold rules ---
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "Distance from Nearest Neighbors" & Edge == "Anthropogenic Edge",
      columns = Logistic
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "Number of Nearest Neighbors" & Edge == "Anthropogenic Edge",
      columns = Segmented
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Feeding" & Edge == "Anthropogenic Edge",
      columns = Linear
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Moving" & Edge == "Anthropogenic Edge",
      columns = Segmented
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Resting" & Edge == "Anthropogenic Edge",
      columns = Null
    )
  ) %>%
  # --- Riparian edge bold rules ---
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "Distance from Nearest Neighbors" & Edge == "Riparian Edge",
      columns = Linear
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "Number of Nearest Neighbors" & Edge == "Riparian Edge",
      columns = Segmented
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Feeding" & Edge == "Riparian Edge",
      columns = Null
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Moving" & Edge == "Riparian Edge",
      columns = Segmented
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Resting" & Edge == "Riparian Edge",
      columns = Null
    )
  ) %>%  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  # left align all column values
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # set equal widths 
  cols_width(
    Response ~ px(150),
    DEI ~ px(150),
    everything() ~ px(100)
  ) %>%   tab_spanner(
    label = "AIC values",
    columns = c(Null, Linear, Power, Logistic, Segmented, Changepoint)
  )




table


# save as html in the viewer
# print and save as pdf in google chrome



