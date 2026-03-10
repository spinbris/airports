library(tidyverse)
library(readxl)
library(lubridate)

otp_path <- "/Users/stephenparton/Library/CloudStorage/OneDrive-SJPConsulting/Documents/Work/virgin_analytics/data_raw/OTP_Time_Series_Master_Current_january_2026.xlsx"

col_names <- c(
  "route",
  "departing_port",
  "arriving_port",
  "airline",
  "month_serial",
  "sectors_scheduled",
  "sectors_flown",
  "cancellations",
  "departures_on_time",
  "arrivals_on_time",
  "departures_delayed",
  "arrivals_delayed",
  "pct_on_time_departures",
  "pct_on_time_arrivals",
  "pct_cancellations"
)

read_otp_sheet <- function(sheet) {
  read_excel(
    otp_path,
    sheet = sheet,
    skip = 1,
    col_names = FALSE,
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "numeric",
      rep("numeric", 7),
      rep("text", 3)
    )
  ) |>
    set_names(col_names) |>
    filter(!is.na(route), !is.na(month_serial)) |>
    mutate(
      date = as.Date(month_serial, origin = "1899-12-30"),
      year = year(date),
      month = month(date),
      across(pct_on_time_departures:pct_cancellations, as.numeric),
      airline = str_to_title(str_trim(airline))
    ) |>
    select(-month_serial)
}

otp_data <- map(excel_sheets(otp_path), read_otp_sheet) |>
  list_rbind() |>
  distinct()

saveRDS(otp_data, "data_raw/otp_data.rds")
