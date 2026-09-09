# API Data Retrieval & Analysis Dashboard
### An R Shiny explorer for the OpenAQ air quality network, built on `openair`

---

## Overview

This app lets you **discover, fetch, analyse, and export** air quality
sensor data from the global [OpenAQ](https://openaq.org) network without
writing any code, then run the same analysis pipeline on your own uploaded
CSV files — including batch-processing many station files at once.

It combines three things into one dashboard:

- **OpenAQ v3 API** — live station and sensor discovery + measurement retrieval
- **[openair](https://davidcarslaw.github.io/openair/) R package** — 15 built-in
  atmospheric analysis / plot types (time series, calendar, wind rose, polar
  plots, trend analysis, and more)
- **Batch file tools** — stack daily exports from one station, or merge files
  from many stations into a single comparative dataset

---

## Tabs

| Tab | Purpose |
|---|---|
| 🔍 **Location Explorer** | Find monitoring stations by city/name, coordinates + radius, bounding box, or country |
| 📥 **Fetch Data** | Tick sensors at a selected station, choose raw/hourly/daily aggregation and a date range, pull measurements into an openair-ready wide table |
| 📊 **Analysis** | Run any of 15 `openair` plot types on fetched data or an uploaded CSV; view model-evaluation statistics; export plots (PNG/PDF/SVG) and data (CSV/RDS/Excel/ZIP) |
| 📦 **Export** | Download the current dataset and metadata (locations, sensors, fetch summary) in multiple formats |
| 🗂 **Batch Processing** | Stack or merge multiple uploaded files, run QC checks, and generate comparative plots across stations |
| ❓ **Help & Guide** | Step-by-step walkthrough, plot descriptions, CSV format spec, troubleshooting table |
| ℹ️ **About** | App identity, feature summary, and data-source/licence details |

---

## Getting Started

1. Register at [explore.openaq.org](https://explore.openaq.org) (free) to get an API key.
2. Paste the key into the sidebar and click **Connect**.
3. In **Location Explorer**, search by city, click the map, or draw a bounding box.
4. Click a map marker or location card to select a station.
5. In **Fetch Data**: tick sensors, choose a date range, click **Fetch**.
6. In **Analysis**: pick a plot type, set its controls, click **Generate Plot**.
7. Download the plot (PNG/PDF/SVG) or the data (CSV/Excel/ZIP).
8. Alternatively, upload your own CSV directly in the Analysis tab, or go to
   **Batch Processing** to combine several files first.

### Required CSV format (for direct upload or batch files)

```
date,pm25,pm10,no2,ws,wd,temp,rh
2024-01-01 00:00,45.2,82.1,18.3,2.1,180,28.5,72
2024-01-01 01:00,48.7,91.3,21.0,1.8,195,27.9,75
```

- `date` — any standard datetime format (auto-detected, or pick a format manually)
- At least one pollutant column (`pm25`, `no2`, `o3`, …)
- `ws` (wind speed, m/s) and `wd` (wind direction, 0–360°) — required only for wind/polar plots
- Column names don't have to match exactly — the mapping panel in Analysis lets you point at the right columns

---

## Batch Processing

Two modes, chosen with a radio button:

### STACK — one station, many files
For daily/periodic export files from the **same** sensor. Each file is
read, its date column auto-detected, and all files are row-bound in
chronological order. Columns missing from some files are filled with `NA`,
duplicate timestamps are dropped (first occurrence kept), and a
`source_file` column records where each row came from.

### MERGE — many stations, one dataset
For one file per station/location — **upload as many files at once as you
like** (multi-select with Ctrl/Cmd+Click). For each file:
- If it already contains its own station/site identifier column
  (`station`, `site`, `location`, `sensor`, `monitor`, or similar), that
  column is detected automatically and used as-is.
- Otherwise, the station label you type for that file (or the filename,
  if left blank) is stamped across every row in that file.

This produces a single dataset with a `station` column, ready for
side-by-side comparison.

**Both modes accept** `.csv`, `.txt`, `.tsv`, auto-detect comma/semicolon
separators and UTF-8/latin1 encoding, and support uploads up to **100 MB**.

After assembling a batch dataset you can:
- Preview it and download as CSV / Excel / RDS
- View per-parameter data completeness and summary statistics (per station, in Merge mode)
- Generate comparative plots across stations (time series, boxplots, mean bar
  charts, correlation heatmaps, calendar plots, Theil-Sen trend, pollution/percentile
  rose, polar plots, and more) and export them as PNG/PDF/SVG
- Send the assembled dataset directly to the Analysis tab for the full `openair` plot suite

---

## Data Source

Air quality data is retrieved live from the [OpenAQ](https://openaq.org)
open-data platform (API v3, `api.openaq.org/v3`), which aggregates
measurements from government reference monitors, low-cost sensors, and
research instruments across 100+ countries. Parameters covered include
PM2.5, PM10, NO₂, O₃, SO₂, CO, NO, BC, temperature, and relative humidity,
among others. Temporal coverage varies by station. Data is licensed
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — free to use with attribution.

---

## Troubleshooting

| Issue | Fix |
|---|---|
| 401 Invalid API key | Re-copy the full key from explore.openaq.org |
| 422 Validation error | Usually an out-of-range radius — max is 25 km |
| 429 Rate limit | Free tier is ~60 req/min; wait 60 s and retry |
| No locations found | Try a different spelling, a larger bounding box, or coordinate search |
| Empty measurement fetch | Widen the date range — some sensors report infrequently |
| Wind rose / polar plot blank | Data needs both `ws` and `wd` columns with valid values |
| Calendar plot shows wrong year | Set the Year control to a year actually present in your data |

---

## Project Structure

```
ui.R              # All UI layout: tabs, inputs, plot/QC panels, upload widgets
batch_helpers.R   # Batch-processing engine: file reading, date/station
                  #   column detection, stacking, merging, QC stats,
                  #   comparative plot dispatch
global.R          # (not included here) shared setup, package loads,
                  #   sourced UI helpers (sec_hdr, scroll_box, status_html)
server.R          # (not included here) reactive logic: API calls, plot
                  #   rendering, dynamic station-label UI, downloads
```

> This README documents the app based on `ui.R` and `batch_helpers.R`.
> `global.R` and `server.R` were not available at the time of writing —
> update this section if their contents affect setup or usage.

---

## Requirements

- R (Shiny app)
- Key packages referenced in the UI/helpers: `shiny`, `shinydashboard`,
  `DT`, `openair`, `dplyr`, plus an OpenAQ API client for the live-fetch tabs
- A free OpenAQ API key from [explore.openaq.org](https://explore.openaq.org)

## Configuration Notes

- Shiny's default upload limit (5 MB) is raised to **100 MB** via
  `options(shiny.maxRequestSize = 100 * 1024^2)` at the top of `ui.R`,
  applying to all file inputs including the batch stack/merge uploaders.
