# Australian Aviation Analysis

Exploratory analysis of Australian aviation data covering airport financials, passenger traffic, and on-time performance.

## Datasets

| Dataset | Source | Coverage |
|---------|--------|----------|
| ACCC Airport Monitoring | ACCC supplementary database | BNE, MEL, PER, SYD — annual 2004–2024 |
| BITRE WebMonthly Traffic | BITRE WebMonthly Airport | 21 airports — monthly Jan 2009–Sep 2025 |
| BITRE On-Time Performance | BITRE OTP Time Series | Route × airline — monthly Jan 2010–Jan 2026 |

## Project Structure

```
.
├── data_raw/                  # Raw Excel files + .rds outputs (gitignored)
├── load_data_airports.R       # Load & clean ACCC airport monitoring data
├── load_data_monthly.R        # Load & clean BITRE monthly traffic data
├── load_data_otp.R            # Load & clean BITRE OTP data
├── eda_airports.R             # Interactive EDA — ACCC financials
├── eda_monthly.R              # Interactive EDA — monthly traffic
├── eda_otp.R                  # Interactive EDA — on-time performance
├── report_airports.qmd        # Quarto report — airport financials
├── report_monthly.qmd         # Quarto report — monthly traffic
└── report_otp.qmd             # Quarto report — on-time performance
```

## Setup

Requires R 4.5+ and [Quarto](https://quarto.org).

```r
# Restore package environment
renv::restore()
```

Place the raw Excel files in `data_raw/`, then run the load scripts to generate `.rds` files before rendering reports:

```bash
Rscript load_data_airports.R
Rscript load_data_monthly.R
Rscript load_data_otp.R
```

## Reports

```bash
quarto render report_airports.qmd
quarto render report_monthly.qmd
quarto render report_otp.qmd
```

Each report renders to a self-contained HTML file (`embed-resources: true`) with folded code blocks.

### Report contents

**`report_airports.qmd`** — ACCC airport financials (annual)
- Passenger volumes and domestic/international split
- Aeronautical revenue, expenses, profit and margin
- EBITA rate of return on aeronautical and total airport assets
- Total airport financials and per-passenger metrics
- Car park and landside financials
- COVID-19 impact and recovery

**`report_monthly.qmd`** — BITRE monthly airport traffic
- Passenger volumes and dom/intl split across all 21 airports
- Aircraft movements and passengers-per-departure proxy
- Seasonality patterns and COVID recovery index
- Bridge: BITRE domestic pax per OTP sector

**`report_otp.qmd`** — BITRE on-time performance
- Airline capacity and market share (sectors flown)
- On-time departure/arrival rates and cancellation rates by airline
- Airport-level OTP trends for ACCC airports
- Bridge: OTP sectors flown vs BITRE domestic outbound movements
- Virgin Australia case study — pre/during/post-administration
- Top 15 routes by volume

## Notes

- OTP data covers **scheduled domestic services only**; BITRE movements include freight, charter and general aviation
- The correct BITRE comparator for OTP sectors is `dom_outbound` (departures only), not `dom_total` (arrivals + departures)
- SYD aeronautical metrics in the ACCC data originally reported as "excluding landfill" variants — normalised to plain names at load time
- SYD car parking operating profit is derived (revenue − expenses) as it is not reported directly in the source data
