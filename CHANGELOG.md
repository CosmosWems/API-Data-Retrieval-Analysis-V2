# Changelog

All notable changes for this release are documented below.


---

## [Version 2.0.0] — Batch File Processing added

### Added

- **New "Batch Processing" tab**, added to the sidebar menu alongside
  Location Explorer, Fetch Data, Analysis, Export, Help & Guide, and About.
- **New `batch_helpers.R` module** containing the whole batch-processing
  engine, independent of the OpenAQ fetch/analysis pipeline:
  - `detect_date_col()` — auto-detects the date/time column in an uploaded file
  - `detect_station_col()` — auto-detects an existing station/site/location/
    sensor/monitor identifier column, if one is already present in the data
  - `read_csv_safe()` — robust CSV/TXT/TSV reader with automatic
    comma/semicolon delimiter and UTF-8/latin1 encoding detection
  - `standardise_dates()` — parses varied date formats to POSIXct
  - `stack_daily_files()` — **STACK mode**: combines multiple daily/periodic
    files from a single station into one continuous, chronologically sorted
    dataset, filling missing columns with `NA` and dropping duplicate
    timestamps
  - `merge_station_files()` — **MERGE mode**: combines one file per
    station/location into a single dataset with a `station` label column,
    enabling side-by-side comparison
  - `station_completeness()`, `station_summary_stats()`,
    `stack_completeness()`, `stack_summary_stats()` — QC / summary
    statistics tables, computed per station in Merge mode
  - `build_station_palette()` — consistent colour assignment per station for plots
  - `run_comparative_plot()` — dispatches 10+ comparative plot types across
    stacked/merged datasets (station time series, boxplots by station, mean
    bar charts, correlation heatmap, calendar plot, Theil-Sen trend,
    trend-level, pollution rose, percentile rose, polar plot, polar annulus)
- **Two processing modes**, selectable via radio buttons:
  - **STACK** — one station, many files (e.g. daily exports from the same sensor)
  - **MERGE** — many stations, one dataset (one file per station/location)
- **Multi-file upload** for both modes — files are selected all at once
  (Ctrl/Cmd+Click) rather than one at a time.
- **Automatic station-name assignment in Merge mode**: if an uploaded file
  already contains its own station/site column, it's detected and used
  as-is; only files with no such column fall back to the typed label (or
  the filename, if left blank).
- **File format support**: `.csv`, `.txt`, `.tsv`, with auto-detected
  delimiter and encoding.
- **Assembled dataset preview** (`DTOutput`) with downloads as CSV, Excel,
  and RDS.
- **QC tables**: per-parameter data completeness (colour-coded thresholds:
  green >80%, yellow 50–80%, red <50%) and summary statistics (mean, SD,
  min, median, max), broken out per station in Merge mode.
- **Comparative Analysis panel**: plot-type selector, dynamic per-plot
  controls, PNG/PDF/SVG export of comparative plots.
- **"Send to Analysis Tab" button**: loads the assembled batch dataset
  directly into the existing single-dataset Analysis tab for the full
  15-plot `openair` suite.
- **Upload size limit raised app-wide** from Shiny's 5 MB default to
  **100 MB**, via `options(shiny.maxRequestSize = 100 * 1024^2)` — required
  for the larger multi-file batch uploads, but applies to every file input
  in the app (including the single-file upload in the Analysis tab).

### Changed

- Sidebar navigation now has 7 items instead of 6 (Batch Processing inserted
  after Export).
- Help text on file inputs updated to state the new 100 MB limit and to
  explain multi-select and auto-detection behaviour.

### Notes

- Batch Processing is additive: the original Location Explorer → Fetch Data
  → Analysis → Export workflow for live OpenAQ data is unchanged. Batch
  Processing is a parallel path for users who already have their own CSV
  exports (from Batch Processing itself, from a sensor's own software, etc.)
  and want to combine/compare them before or instead of using the live
  fetch tab.
- No breaking changes to existing OpenAQ fetch or single-file Analysis
  functionality were identified in the reviewed files.
