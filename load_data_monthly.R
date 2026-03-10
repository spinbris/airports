library(tidyverse)
library(readxl)
library(lubridate)

web_path <- "/Users/stephenparton/Library/CloudStorage/OneDrive-SJPConsulting/Documents/Work/virgin_analytics/data_raw/WebMonthlyAirportSeptember2025.xlsx"

# Airport passengers -------------------------------------------------------
# Source sheet has 5 preamble rows, then a two-row merged header (rows 6-7),
# then data from row 8 onwards.

airport_pax <- read_excel(web_path, sheet = "Airport Passengers", skip = 7, col_names = FALSE) |>
  set_names(c(
    "airport", "year", "month",
    "dom_inbound", "dom_outbound", "dom_total",
    "intl_inbound", "intl_outbound", "intl_total",
    "total_inbound", "total_outbound", "total_total"
  )) |>
  filter(!is.na(airport), !is.na(year)) |>
  mutate(
    across(c(year, month), as.integer),
    across(dom_inbound:total_total, as.numeric),
    date = make_date(year, month, 1L)
  ) |>
  select(airport, date, year, month, everything())

# Aircraft movements -------------------------------------------------------
# Source sheet has 5 preamble rows, then a single header row (row 6),
# then data from row 7 onwards.

airport_movements <- read_excel(web_path, sheet = "AircraftMovements", skip = 6, col_names = FALSE) |>
  set_names(c(
    "airport", "year", "month",
    "dom_inbound", "dom_outbound", "dom_total",
    "intl_inbound", "intl_outbound", "intl_total",
    "total_inbound", "total_outbound", "total_total"
  )) |>
  filter(!is.na(airport), !is.na(year)) |>
  mutate(
    across(c(year, month), as.integer),
    across(dom_inbound:total_total, as.numeric),
    date = make_date(year, month, 1L)
  ) |>
  select(airport, date, year, month, everything())

# Save to RDS --------------------------------------------------------------
saveRDS(airport_pax,       "data_raw/airport_pax.rds")
saveRDS(airport_movements, "data_raw/airport_movements.rds")
