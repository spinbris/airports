library(tidyverse)
library(readxl)

path <- "/Users/stephenparton/Library/CloudStorage/OneDrive-SJPConsulting/Documents/Work/virgin_analytics/data_raw/accc-airport-monitoring-report-2023-24-supplementary-database_0.xlsx"

# Helper: read a single sheet and pivot to long format
read_airport_sheet <- function(airport, sheet_name, category_label) {
  read_excel(path, sheet = sheet_name, col_types = "text") |>
    rename(metric = 1) |>
    filter(!is.na(metric)) |>
    pivot_longer(-metric, names_to = "year", values_to = "value") |>
    mutate(
      airport = airport,
      category = category_label,
      value = suppressWarnings(as.numeric(value))
    ) |>
    select(airport, category, year, metric, value)
}

# Sheet mapping
sheet_map <- tibble(
  airport = c("BNE", "BNE", "MEL", "MEL", "PER", "PER", "SYD", "SYD"),
  sheet = c(
    "BNE total and aero",
    "BNE Car park and Landside",
    "MEL total and aero",
    "MEL Car park and Landside",
    "PER total and aero",
    "PER Car park and landside",
    "SYD total and aero",
    "SYD Car park and landside"
  ),
  category = c(
    "total_aero",
    "carpark_landside",
    "total_aero",
    "carpark_landside",
    "total_aero",
    "carpark_landside",
    "total_aero",
    "carpark_landside"
  )
)

# Read and combine all sheets
accc_data <- pmap(sheet_map, \(airport, sheet, category) {
  read_airport_sheet(airport, sheet, category)
}) |>
  list_rbind() |>
  # Normalise year separator to plain hyphen
  mutate(year = str_replace(year, "[\u2013\u2014\u2212-]", "-")) |>
  # Normalise SYD landfill-adjusted metric names to match other airports
  mutate(metric = str_remove(metric, " - excluding landfill"))

# Derive SYD car parking operating profit (revenue - expenses) as it is not
# reported directly in the source data
syd_cp_profit <- accc_data |>
  filter(
    airport == "SYD",
    category == "carpark_landside",
    metric %in%
      c(
        "Car parking revenue ($million)",
        "Car parking operating expenses ($million)"
      )
  ) |>
  pivot_wider(names_from = metric, values_from = value) |>
  mutate(
    `Car parking operating profit ($million)` = `Car parking revenue ($million)` -
      `Car parking operating expenses ($million)`
  ) |>
  select(airport, category, year, `Car parking operating profit ($million)`) |>
  pivot_longer(
    `Car parking operating profit ($million)`,
    names_to = "metric",
    values_to = "value"
  )

accc_data <- bind_rows(accc_data, syd_cp_profit)

# Save to RDS for use in other scripts
saveRDS(accc_data, "data_raw/accc_data.rds")
