library(knitr)
library(dplyr)
library(gt)
library(webshot2)

results <- tribble(
  ~Response, ~Null, ~Linear, ~Power,  ~Logistic,  ~Segmented, ~Changepoint, ~PseudoR2,    ~DEI,
  "(A) Distance from Nearest Neighbors", 70.57, 68.77,   65.95, 67.96, 67.73, 75.81, 0.27, "357 (292-376)",
  "(A) Number of Nearest Neighbors", 24.42, 23.5,   24.31, 26.39, 25.71, 40.43, 0.10, "252 (17-368)",
  "(A) % Time Feeding", 159.1, 152.43,   153.16, 155.22, 154.52, 173.03, 0.27, "278 (38-405)",
  "(A) % Time Moving", 143.33, 145.33,   147.32, 147.58, 148.46, 154.4, 0.00, "No Edge Effect",
  "(A) % Time Resting", 169.49, 168.41,   170.11, 164.76, 171.12, 182.37, 0.32,  "121 (62-179)",
  "(R) Distance from Nearest Neighbors", 91.34, 83.14,   83.39, 84.35, 83.62, 112.14, 0.29,  "327 (37-467)",
  "(R) Number of Nearest Neighbors", 26.46, 15.2,   17.19, 11.49, 17.03, 30.23, 0.50, "148 (121-175)",
  "(R) % Time Feeding", 217.73, 218.9,   220.37, 220.66, 218.51, 232.91, 0.00, "No Edge Effect",
  "(R) % Time Moving", 184.56, 185.13,   182.07, 181.64, 179.75, 234.04, 0.29,  "238 (110-365)",
  "(R) % Time Resting", 234.15, 233.59,   235.59, 236.35, 235.13, 265.19, 0.08,  "327 (24-469)"
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
      columns = Power
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "Number of Nearest Neighbors" & Edge == "Anthropogenic Edge",
      columns = Linear
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
      columns = Null
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Response == "% Time Resting" & Edge == "Anthropogenic Edge",
      columns = Logistic
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
      columns = Logistic
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
      columns = Linear
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



