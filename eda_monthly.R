library(tidyverse)
library(lubridate)
library(scales)

airport_pax <- readRDS("data_raw/airport_pax.rds")
airport_mvt <- readRDS("data_raw/airport_movements.rds")

# Consistent airport ordering for plots
airport_order <- c("SYDNEY", "MELBOURNE", "BRISBANE", "PERTH")

accc_airports <- airport_pax |>
  filter(airport %in% airport_order) |>
  mutate(airport = factor(airport, levels = airport_order))

accc_mvt <- airport_mvt |>
  filter(airport %in% airport_order) |>
  mutate(airport = factor(airport, levels = airport_order))

# -------------------------------------------------------------------
# 1. Data overview
# -------------------------------------------------------------------

airport_pax |>
  summarise(
    airports = n_distinct(airport),
    from = min(date),
    to = max(date),
    rows = n()
  )

# Passenger totals by airport (all years)
airport_pax |>
  summarise(
    total_pax = sum(total_total, na.rm = TRUE),
    dom_pax = sum(dom_total, na.rm = TRUE),
    intl_pax = sum(intl_total, na.rm = TRUE),
    .by = airport
  ) |>
  arrange(desc(total_pax))

# -------------------------------------------------------------------
# 2. Total passenger volumes — monthly trend (ACCC airports)
# -------------------------------------------------------------------

accc_airports |>
  ggplot(aes(x = date, y = total_total, colour = airport)) +
  geom_line(alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total monthly passengers — ACCC airports",
    x = NULL,
    y = "Passengers",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 3. Domestic vs international split over time
# -------------------------------------------------------------------

accc_airports |>
  select(airport, date, dom_total, intl_total) |>
  pivot_longer(
    c(dom_total, intl_total),
    names_to = "type",
    values_to = "pax"
  ) |>
  mutate(type = if_else(type == "dom_total", "Domestic", "International")) |>
  ggplot(aes(x = date, y = pax, colour = type)) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~airport, scales = "free_y") +
  labs(
    title = "Domestic vs international passengers — ACCC airports",
    x = NULL,
    y = "Passengers",
    colour = NULL
  )

# -------------------------------------------------------------------
# 4. International share of total passengers over time
# -------------------------------------------------------------------

accc_airports |>
  mutate(intl_share = intl_total / total_total) |>
  ggplot(aes(x = date, y = intl_share, colour = airport)) +
  geom_line(alpha = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "International share of total passengers — ACCC airports",
    x = NULL,
    y = "International share (%)",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 5. Annual passenger volumes — all reporting airports
# -------------------------------------------------------------------

airport_pax |>
  summarise(
    total_pax = sum(total_total, na.rm = TRUE),
    .by = c(airport, year)
  ) |>
  mutate(airport = fct_reorder(airport, total_pax, .fun = sum, .desc = TRUE)) |>
  ggplot(aes(x = year, y = total_pax, colour = airport)) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Annual total passengers — all reporting airports",
    x = NULL,
    y = "Passengers",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 6. Aircraft movements — domestic vs international (ACCC airports)
# -------------------------------------------------------------------

accc_mvt |>
  select(airport, date, dom_outbound, intl_outbound) |>
  pivot_longer(
    c(dom_outbound, intl_outbound),
    names_to = "type",
    values_to = "movements"
  ) |>
  mutate(type = if_else(type == "dom_outbound", "Domestic", "International")) |>
  ggplot(aes(x = date, y = movements, colour = type)) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~airport, scales = "free_y") +
  labs(
    title = "Domestic vs international outbound movements — ACCC airports",
    x = NULL,
    y = "Aircraft movements",
    colour = NULL
  )

# -------------------------------------------------------------------
# 7. Passengers per movement (load proxy) — ACCC airports
# -------------------------------------------------------------------
# Dom pax / dom outbound movements = implied passengers per departure.
# Not true load factor (no seat count) but a useful capacity-utilisation proxy.

accc_airports |>
  left_join(
    accc_mvt |>
      select(
        airport,
        date,
        mvt_dom_out = dom_outbound,
        mvt_intl_out = intl_outbound
      ),
    join_by(airport, date)
  ) |>
  mutate(
    pax_per_dom_dep = dom_total / mvt_dom_out,
    pax_per_intl_dep = intl_total / mvt_intl_out
  ) |>
  select(airport, date, pax_per_dom_dep, pax_per_intl_dep) |>
  pivot_longer(
    c(pax_per_dom_dep, pax_per_intl_dep),
    names_to = "type",
    values_to = "pax_per_dep"
  ) |>
  mutate(
    type = if_else(type == "pax_per_dom_dep", "Domestic", "International")
  ) |>
  ggplot(aes(x = date, y = pax_per_dep, colour = airport)) +
  geom_line(alpha = 0.7) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~type, scales = "free_y") +
  labs(
    title = "Passengers per departure (load proxy) — ACCC airports",
    subtitle = "Higher = fuller planes / larger aircraft",
    x = NULL,
    y = "Passengers per departure",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 8. Seasonality — average passengers by month (domestic, pre-COVID)
# -------------------------------------------------------------------

accc_airports |>
  filter(year < 2020) |>
  summarise(avg_pax = mean(dom_total, na.rm = TRUE), .by = c(airport, month)) |>
  mutate(month_label = month(month, label = TRUE)) |>
  ggplot(aes(x = month_label, y = avg_pax, colour = airport, group = airport)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Average domestic passengers by month — pre-COVID seasonality",
    subtitle = "2009–2019 average",
    x = NULL,
    y = "Average passengers",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 9. COVID-19 impact and recovery — indexed to Jan 2020
# -------------------------------------------------------------------

# Index total passengers to Jan 2020 = 100
base_jan2020 <- accc_airports |>
  filter(year == 2020, month == 1) |>
  select(airport, base = total_total)

accc_airports |>
  filter(date >= as.Date("2019-01-01")) |>
  left_join(base_jan2020, join_by(airport)) |>
  mutate(index = total_total / base * 100) |>
  ggplot(aes(x = date, y = index, colour = airport)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  labs(
    title = "Passenger recovery post-COVID — indexed to Jan 2020 = 100",
    x = NULL,
    y = "Index (Jan 2020 = 100)",
    colour = "Airport"
  )

# -------------------------------------------------------------------
# 10. Bridge — annual pax vs OTP sectors flown (ACCC airports)
# -------------------------------------------------------------------
# Connects monthly BITRE pax to OTP airline-level data.
# Domestic pax from BITRE vs domestic sectors flown from OTP by year.

otp_data <- readRDS("data_raw/otp_data.rds")

port_to_airport <- c(
  "Sydney" = "SYDNEY",
  "Melbourne" = "MELBOURNE",
  "Brisbane" = "BRISBANE",
  "Perth" = "PERTH"
)

otp_annual <- otp_data |>
  filter(
    airline == "All Airlines",
    departing_port %in% names(port_to_airport),
  ) |>
  summarise(
    sectors_flown = sum(sectors_flown, na.rm = TRUE),
    .by = c(departing_port, year)
  ) |>
  mutate(airport = port_to_airport[departing_port])

bitre_annual <- accc_airports |>
  summarise(dom_pax = sum(dom_total, na.rm = TRUE), .by = c(airport, year)) |>
  mutate(airport = as.character(airport))

bridge <- otp_annual |>
  left_join(bitre_annual, join_by(airport, year)) |>
  mutate(pax_per_sector = dom_pax / sectors_flown)

bridge |>
  ggplot(aes(x = year, y = pax_per_sector, colour = airport)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Domestic passengers per OTP sector — ACCC airports (annual)",
    subtitle = "BITRE dom pax ÷ OTP sectors flown. Reflects aircraft size & load trends.",
    x = NULL,
    y = "Passengers per sector",
    colour = "Airport"
  )
