library(tidyverse)
library(lubridate)
library(scales)

otp_data <- readRDS("data_raw/otp_data.rds")
airport_pax <- readRDS("data_raw/airport_pax.rds")
airport_mvt <- readRDS("data_raw/airport_movements.rds")

# Key airline filter — major carriers only (exclude "All Airlines" aggregate)
major_airlines <- c(
  "Qantas",
  "Jetstar",
  "Virgin Australia",
  "Virgin Australia Regional Airlines",
  "Tigerair Australia",
  "Rex Airlines",
  "Regional Express"
)

# OTP data filtered to major airlines
otp_major <- otp_data |>
  filter(airline %in% major_airlines)

# Airport-level aggregation helper — sum OTP metrics by departing port + date
otp_by_airport <- otp_data |>
  filter(airline == "All Airlines") |>
  summarise(
    sectors_scheduled = sum(sectors_scheduled, na.rm = TRUE),
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    cancellations = sum(cancellations, na.rm = TRUE),
    departures_on_time = sum(departures_on_time, na.rm = TRUE),
    .by = c(departing_port, date, year, month)
  ) |>
  mutate(
    pct_on_time = departures_on_time / sectors_flown * 100,
    pct_cancelled = cancellations / sectors_scheduled * 100
  )

# -------------------------------------------------------------------
# 1. Data overview
# -------------------------------------------------------------------

# Rows by airline
otp_data |>
  filter(airline != "All Airlines") |>
  count(airline, sort = TRUE)

# Date coverage by airline
otp_data |>
  filter(airline %in% major_airlines) |>
  summarise(from = min(date), to = max(date), .by = airline) |>
  arrange(from)

# -------------------------------------------------------------------
# 2. Sectors flown by airline over time (annual)
# -------------------------------------------------------------------

otp_major |>
  filter(route != "All Ports-All Ports") |>
  summarise(
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    .by = c(airline, year)
  ) |>
  ggplot(aes(x = year, y = sectors_flown, colour = airline)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Sectors flown by airline (annual)",
    x = NULL,
    y = "Sectors flown",
    colour = "Airline"
  )

# -------------------------------------------------------------------
# 3. Market share of sectors flown by airline (annual)
# -------------------------------------------------------------------

otp_major |>
  summarise(
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    .by = c(airline, year)
  ) |>
  mutate(share = sectors_flown / sum(sectors_flown), .by = year) |>
  ggplot(aes(x = year, y = share, fill = airline)) +
  geom_area() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Domestic market share — sectors flown",
    x = NULL,
    y = "Share of sectors",
    fill = "Airline"
  )

# -------------------------------------------------------------------
# 4. On-time departure performance by airline over time
# -------------------------------------------------------------------

otp_major |>
  summarise(
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    departures_on_time = sum(departures_on_time, na.rm = TRUE),
    .by = c(airline, year)
  ) |>
  mutate(pct_on_time = departures_on_time / sectors_flown * 100) |>
  ggplot(aes(x = year, y = pct_on_time, colour = airline)) +
  geom_line() +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "grey50") +
  labs(
    title = "On-time departure performance by airline (annual)",
    subtitle = "Dashed line = 80% threshold",
    x = NULL,
    y = "On-time departures (%)",
    colour = "Airline"
  )

# -------------------------------------------------------------------
# 5. Cancellation rate by airline over time
# -------------------------------------------------------------------

otp_major |>
  summarise(
    sectors_scheduled = sum(sectors_scheduled, na.rm = TRUE),
    cancellations = sum(cancellations, na.rm = TRUE),
    .by = c(airline, year)
  ) |>
  mutate(pct_cancelled = cancellations / sectors_scheduled * 100) |>
  ggplot(aes(x = year, y = pct_cancelled, colour = airline)) +
  geom_line() +
  labs(
    title = "Cancellation rate by airline (annual)",
    x = NULL,
    y = "Cancellations (%)",
    colour = "Airline"
  )

# -------------------------------------------------------------------
# 6. OTP by airport (departing port) — link to airport-level data
# -------------------------------------------------------------------

# Focus on the 4 ACCC-monitored airports
accc_ports <- c("Sydney", "Melbourne", "Brisbane", "Perth")

otp_by_airport |>
  filter(departing_port %in% accc_ports) |>
  ggplot(aes(x = date, y = pct_on_time, colour = departing_port)) +
  geom_line(alpha = 0.4) +
  geom_smooth(se = FALSE, span = 0.15) +
  labs(
    title = "On-time departure performance — ACCC airports (all airlines)",
    x = NULL,
    y = "On-time departures (%)",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 7. Tie OTP movements to airport_mvt — sectors flown vs dom outbound
# -------------------------------------------------------------------
# OTP covers scheduled domestic departures only.
# BITRE dom_outbound is the correct comparator (departures only).
# Residual gap ~10-20% = freight, charter & general aviation not in OTP.
# Note: dom_total double-counts (arrivals + departures), so is ~2x OTP.

# Aggregate OTP sectors to monthly by departing port
otp_sectors_monthly <- otp_data |>
  filter(airline == "All Airlines") |>
  summarise(
    sectors_scheduled = sum(sectors_scheduled, na.rm = TRUE),
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    .by = c(departing_port, date)
  )

# Map departing port names to airport_mvt airport names (title case vs upper)
port_to_airport <- c(
  "Sydney" = "SYDNEY",
  "Melbourne" = "MELBOURNE",
  "Brisbane" = "BRISBANE",
  "Perth" = "PERTH"
)

# Join OTP sectors to dom outbound movements for ACCC airports
otp_vs_mvt <- otp_sectors_monthly |>
  filter(departing_port %in% names(port_to_airport)) |>
  mutate(airport = port_to_airport[departing_port]) |>
  left_join(
    airport_mvt |> select(airport, date, dom_outbound),
    join_by(airport, date)
  )

# Compare sectors flown (OTP) vs domestic outbound movements (BITRE)
otp_vs_mvt |>
  pivot_longer(
    c(sectors_flown, dom_outbound),
    names_to = "source",
    values_to = "movements"
  ) |>
  mutate(
    source = if_else(
      source == "sectors_flown",
      "OTP (sectors flown, scheduled)",
      "BITRE (dom outbound, all traffic)"
    )
  ) |>
  ggplot(aes(x = date, y = movements, colour = source)) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~departing_port, scales = "free_y") +
  labs(
    title = "OTP sectors flown vs BITRE domestic outbound movements",
    subtitle = "ACCC airports — monthly. Gap ≈ freight, charter & general aviation.",
    x = NULL,
    y = "Movements / sectors",
    colour = "Source"
  )

# -------------------------------------------------------------------
# 8. Virgin Australia OTP — pre/post administration (Apr 2020)
# -------------------------------------------------------------------

va_otp <- otp_data |>
  filter(str_detect(airline, "Virgin Australia")) |>
  summarise(
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    departures_on_time = sum(departures_on_time, na.rm = TRUE),
    cancellations = sum(cancellations, na.rm = TRUE),
    sectors_scheduled = sum(sectors_scheduled, na.rm = TRUE),
    .by = date
  ) |>
  mutate(
    pct_on_time = departures_on_time / sectors_flown * 100,
    pct_cancelled = cancellations / sectors_scheduled * 100,
    era = case_when(
      date < as.Date("2020-04-01") ~ "Pre-administration",
      date < as.Date("2021-01-01") ~ "Administration / restart",
      TRUE ~ "Post-administration"
    )
  )

ggplot(va_otp, aes(x = date, y = pct_on_time, colour = era, group = 1)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed") +
  scale_colour_manual(
    values = c(
      "Pre-administration" = "#2166ac",
      "Administration / restart" = "#d7191c",
      "Post-administration" = "#1a9641"
    )
  ) +
  labs(
    title = "Virgin Australia — on-time departure performance",
    subtitle = "Dashed line = administration (Apr 2020)",
    x = NULL,
    y = "On-time departures (%)",
    colour = NULL
  )

# -------------------------------------------------------------------
# 9. Top routes by sectors flown (most recent full year)
# -------------------------------------------------------------------

otp_data |>
  filter(
    airline == "All Airlines",
    year == max(year) - 1,
    route != "All Ports-All Ports",
  ) |>
  summarise(sectors_flown = sum(sectors_flown, na.rm = TRUE), .by = route) |>
  slice_max(sectors_flown, n = 15) |>
  mutate(route = fct_reorder(route, sectors_flown)) |>
  ggplot(aes(x = sectors_flown, y = route)) +
  geom_col() +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Top 15 routes by sectors flown",
    x = "Sectors flown",
    y = NULL
  )
