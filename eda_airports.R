library(tidyverse)

# Load data ---------------------------------------------------------------

accc_data <- readRDS("data_raw/accc_data.rds")

# Helper: extract a single metric across all airports/years
get_metric <- function(data, metric_pattern, cat = NULL) {
  out <- data |> filter(str_detect(metric, metric_pattern))
  if (!is.null(cat)) {
    out <- out |> filter(category == cat)
  }
  out
}

# 1. Data overview --------------------------------------------------------

# Shape, completeness and coverage
accc_data |>
  summarise(
    rows = n(),
    airports = n_distinct(airport),
    categories = n_distinct(category),
    metrics = n_distinct(metric),
    years = n_distinct(year),
    pct_na = mean(is.na(value)) * 100
  )

# Distinct years in order
accc_data |> distinct(year) |> arrange(year)

# Row counts by airport x category
accc_data |>
  count(airport, category) |>
  pivot_wider(names_from = category, values_from = n)

# 2. Passenger volumes ----------------------------------------------------

passengers <- get_metric(accc_data, "^Passengers \\('000\\)$", "total_aero")

ggplot(
  passengers,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Total passengers by airport (2004-05 to 2023-24)",
    x = NULL,
    y = "Passengers ('000)",
    colour = "Airport"
  )

# Domestic vs international split (most recent year)
dom_intl <- accc_data |>
  filter(
    metric %in%
      c("Domestic Passengers ('000)", "International Passengers ('000)"),
    category == "total_aero",
    year == "2023-24"
  )

ggplot(dom_intl, aes(x = airport, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Domestic vs International passengers by airport (2023-24)",
    x = "Airport",
    y = "Passengers ('000)",
    fill = NULL
  )

# 3. Aeronautical financials ----------------------------------------------

# Revenue, expenses, profit over time.
aero_financials <- accc_data |>
  filter(
    category == "total_aero",
    str_detect(
      metric,
      regex(
        "^aeronautical (revenue|operating expenses|operating profit) \\(\\$m\\)$",
        ignore_case = TRUE
      )
    )
  )

ggplot(
  aero_financials,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Aeronautical revenue, expenses and profit by airport",
    x = NULL,
    y = "$million",
    colour = "Airport"
  )

# Profit margin over time
aero_margin <- accc_data |>
  filter(
    category == "total_aero",
    str_detect(
      metric,
      regex("^aeronautical profit margin \\(%\\)$", ignore_case = TRUE)
    )
  )

ggplot(
  aero_margin,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Aeronautical profit margin by airport (%)",
    x = NULL,
    y = "Profit margin (%)",
    colour = "Airport"
  )

# Per-passenger metrics (latest year)
per_pax <- accc_data |>
  filter(
    category == "total_aero",
    metric %in%
      c(
        "Aeronautical Revenue per passenger ($)",
        "Aeronautical Expenses per passenger ($)",
        "Aeronautical Operating profit per passenger ($)"
      ),
    year == "2023-24"
  )

ggplot(per_pax, aes(x = airport, y = value, fill = airport)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    title = "Aeronautical per-passenger metrics by airport (2023-24)",
    x = NULL,
    y = "$",
    fill = "Airport"
  )

# 4. Total airport financials ---------------------------------------------

total_financials <- accc_data |>
  filter(
    category == "total_aero",
    metric %in%
      c(
        "Total airport revenue ($m)",
        "Total airport operating expenses ($m)",
        "Total airport operating profit ($m)"
      )
  )

ggplot(
  total_financials,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Total airport financials by airport",
    x = NULL,
    y = "$million",
    colour = "Airport"
  )

# Rate of return on aeronautical assets
ror <- get_metric(
  accc_data,
  "^Rate of return \\(EBITA\\) on tangible non-current assets for aeronautical services \\(%\\)$",
  "total_aero"
)

ggplot(ror, aes(x = year, y = value, colour = airport, group = airport)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Rate of return (EBITA) on aeronautical tangible assets (%)",
    x = NULL,
    y = "Rate of return (%)",
    colour = "Airport"
  )

# 5. Car park financials --------------------------------------------------

carpark_fin <- accc_data |>
  filter(
    category == "carpark_landside",
    metric %in%
      c(
        "Car parking revenue ($million)",
        "Car parking operating expenses ($million)",
        "Car parking operating profit ($million)"
      )
  )

ggplot(
  carpark_fin,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Car parking financials by airport",
    x = NULL,
    y = "$million",
    colour = "Airport"
  )

# Car park profit margin
cp_margin <- get_metric(
  accc_data,
  "^Car parking profit margin",
  "carpark_landside"
)

ggplot(cp_margin, aes(x = year, y = value, colour = airport, group = airport)) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Car parking profit margin by airport (%)",
    x = NULL,
    y = "Profit margin (%)",
    colour = "Airport"
  )

# Revenue per vehicle
rev_per_veh <- get_metric(accc_data, "^Revenue per vehicle", "carpark_landside")

ggplot(
  rev_per_veh,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Car parking revenue per vehicle by airport ($)",
    x = NULL,
    y = "$",
    colour = "Airport"
  )

# 6. Landside access revenue ----------------------------------------------

landside_rev <- get_metric(
  accc_data,
  "^Total landside access revenue",
  "carpark_landside"
)

ggplot(
  landside_rev,
  aes(x = year, y = value, colour = airport, group = airport)
) +
  geom_line() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title = "Total landside access revenue by airport ($ millions)",
    x = NULL,
    y = "$ millions",
    colour = "Airport"
  )

# 7. COVID-19 impact summary (2019-20 to 2021-22) -------------------------

covid_pax <- passengers |>
  filter(year %in% c("2018-19", "2019-20", "2020-21", "2021-22", "2022-23")) |>
  select(airport, year, value) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(
    pct_drop_covid = (`2020-21` - `2018-19`) / `2018-19` * 100,
    pct_recovery_23 = (`2022-23` - `2018-19`) / `2018-19` * 100
  )

covid_pax
