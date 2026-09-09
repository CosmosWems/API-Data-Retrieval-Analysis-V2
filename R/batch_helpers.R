# ================================================================
#  R/batch_helpers.R  –  Batch file processing & comparative analysis
#
#  TWO modes
#  ─────────
#  STACK  – multiple CSV files from the SAME station covering
#            different days/periods are read and row-bound into a
#            single chronological data.frame.  Gaps between files
#            are preserved as missing rows; duplicate timestamps
#            are de-duplicated (first occurrence kept).
#
#  MERGE  – CSV files from DIFFERENT stations are row-bound after
#            adding a user-defined station label column so that
#            faceted / grouped comparative analysis can be run.
#
#  Comparative plot types (run_comparative_plot)
#  ─────────────────────────────────────────────
#  station_timeseries   multi-line time series, one line per station
#  boxplot_station      box plots per station per parameter
#  mean_bar             mean bar chart per station per parameter
#  correlation_heatmap  Pearson r matrix between stations
#  + all openair plots via type = station_col injection
# ================================================================

`%||%` <- function(a, b)
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ── Detect the datetime column ────────────────────────────────
detect_date_col <- function(df) {
  cands <- names(df)
  exact <- c("date","Date","datetime","DateTime","date_time",
             "timestamp","Timestamp","time","Time","DATE","DATETIME",
             "utc","UTC","local","Local")
  hit <- intersect(exact, cands)
  if (length(hit)) return(hit[1])
  pat <- cands[grepl("date|time|stamp", cands, ignore.case=TRUE)]
  if (length(pat)) return(pat[1])
  for (col in cands) {
    vals <- na.omit(as.character(df[[col]]))[seq_len(min(5, nrow(df)))]
    if (!length(vals)) next
    ok <- suppressWarnings(
      lubridate::parse_date_time(vals,
                                 orders=c("YmdHMSz","YmdHMS","Ymd HMS","dmy HMS",
                                          "mdy HMS","dmy HM","mdy HM","Ymd","dmy","mdy"),
                                 quiet=TRUE))
    if (sum(!is.na(ok)) >= length(vals)*0.8) return(col)
  }
  NULL
}

# ── Robust CSV reader ─────────────────────────────────────────
read_csv_safe <- function(path) {
  hdr <- tryCatch(readLines(path, n=2, warn=FALSE),
                  error=function(e) readLines(path, n=2, warn=FALSE,
                                              encoding="latin1"))
  sep <- if (any(grepl(";", hdr, fixed=TRUE))) ";" else ","
  tryCatch(
    read.csv(path, sep=sep, stringsAsFactors=FALSE,
             encoding="UTF-8", check.names=FALSE),
    error=function(e)
      tryCatch(
        read.csv(path, sep=sep, stringsAsFactors=FALSE,
                 fileEncoding="latin1", check.names=FALSE),
        error=function(e2)
          stop(paste0("Cannot read '", basename(path), "': ", e2$message))
      )
  )
}

# ── Detect an existing station / site identifier column ──────
detect_station_col <- function(df, date_col=NULL) {
  cands <- setdiff(names(df), date_col)
  exact <- c("station","Station","STATION",
             "site","Site","SITE",
             "station_name","Station_Name","StationName",
             "site_name","Site_Name","SiteName",
             "location","Location","LOCATION",
             "sensor","Sensor","SENSOR",
             "monitor","Monitor","MONITOR",
             "station_id","Station_ID","StationID",
             "site_id","Site_ID","SiteID")
  hit <- intersect(exact, cands)
  if (length(hit)) return(hit[1])
  pat <- cands[grepl("station|site.?name|location|sensor|monitor",
                     cands, ignore.case=TRUE)]
  if (length(pat)) return(pat[1])
  NULL
}

# ── Parse dates to POSIXct ────────────────────────────────────
standardise_dates <- function(x, tz="UTC") {
  if (inherits(x,"POSIXct")) return(x)
  x <- as.character(x)
  p <- suppressWarnings(lubridate::parse_date_time(x,
                                                   orders=c("YmdHMSz","YmdHMS","Ymd HMS","Ymd HM",
                                                            "dmy HMS","dmy HM","mdy HMS","mdy HM",
                                                            "Ymd","dmy","mdy","ymd"), tz=tz, quiet=TRUE))
  if (sum(is.na(p)) > length(p)*0.5)
    p <- suppressWarnings(as.POSIXct(x, tz=tz))
  p
}

# ── Coerce non-date columns to numeric where >30 % parse ─────
.coerce_numeric <- function(df, skip=c("date","station","source_file")) {
  for (col in setdiff(names(df), skip)) {
    v <- suppressWarnings(as.numeric(df[[col]]))
    if (sum(!is.na(v)) > sum(!is.na(df[[col]]))*0.3)
      df[[col]] <- v
  }
  df
}

# ── Union-bind a list of data.frames (fill missing with NA) ──
.union_bind <- function(parts) {
  all_cols <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop=FALSE]
  })
  dplyr::bind_rows(parts)
}

# ================================================================
#  stack_daily_files
#  Inputs:
#    file_df  – data.frame from fileInput() ($datapath, $name)
#    tz       – timezone for date parsing
#  Returns:
#    data.frame sorted by date, source_file column added,
#    duplicate timestamps removed (first kept).
# ================================================================
stack_daily_files <- function(file_df, tz="UTC") {
  if (is.null(file_df) || nrow(file_df)==0)
    stop("No files provided for stacking.")
  
  parts  <- vector("list", nrow(file_df))
  errors <- character(0)
  
  for (i in seq_len(nrow(file_df))) {
    path  <- file_df$datapath[i]
    fname <- file_df$name[i]
    tryCatch({
      df <- read_csv_safe(path)
      if (nrow(df)==0) stop("File is empty.")
      dcol <- detect_date_col(df)
      if (is.null(dcol)) stop("No date column detected.")
      df$date <- standardise_dates(df[[dcol]], tz)
      if (dcol != "date") df[[dcol]] <- NULL
      df <- .coerce_numeric(df)
      df$source_file <- fname
      parts[[i]] <- df
    }, error=function(e)
      errors <<- c(errors, paste0("[", fname, "] ", e$message)))
  }
  
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts))
    stop(paste0("All files failed:\n", paste(errors, collapse="\n")))
  
  out <- .union_bind(parts)
  out <- out[!is.na(out$date), , drop=FALSE]
  out <- dplyr::arrange(out, date)
  
  # De-duplicate exact timestamps (keep first occurrence per source)
  out <- dplyr::distinct(out, date, source_file, .keep_all=TRUE)
  
  if (length(errors)) attr(out, "load_warnings") <- errors
  out
}

# ================================================================
#  merge_station_files
#  Inputs:
#    file_df       – data.frame from fileInput() ($datapath, $name),
#                    ANY number of rows (one row per uploaded file)
#    station_names – character vector, one *fallback* label per file,
#                    used only for rows where the file itself carries
#                    no station/site identifier column. May contain
#                    blanks — blanks are filled from the filename.
#    station_col   – name of the new station column (default "station")
#    tz            – timezone
#  Behaviour:
#    For each file, we look for an existing station/site-type column
#    (see detect_station_col()). If one is found and has non-blank
#    values, those per-row values are kept (renamed to station_col);
#    any blank rows within that column fall back to the file-level
#    label. If no such column exists at all, every row in the file is
#    stamped with the file-level label (typed name, or the filename
#    with its extension stripped if left blank).
#  Returns:
#    data.frame with station_col prepended, sorted station then date.
#    attr(out, "station_auto_detected") is a named logical vector
#    (one entry per input file) indicating which files already had
#    their own station/site column, for surfacing in the UI.
# ================================================================
merge_station_files <- function(file_df, station_names,
                                station_col="station", tz="UTC") {
  if (is.null(file_df) || nrow(file_df)==0)
    stop("No files provided for merging.")
  if (length(station_names) != nrow(file_df))
    stop("station_names must have same length as number of files.")
  
  # Fill blank fallback names with filenames
  station_names <- trimws(station_names)
  blank <- station_names=="" | is.na(station_names)
  station_names[blank] <- tools::file_path_sans_ext(file_df$name[blank])
  
  # Only disambiguate the *fallback* labels — files that carry their
  # own station column keep whatever real names are already inside them
  fallback_names <- station_names
  if (anyDuplicated(fallback_names))
    fallback_names <- make.unique(fallback_names, sep="_")
  
  parts    <- vector("list", nrow(file_df))
  errors   <- character(0)
  detected <- stats::setNames(logical(nrow(file_df)), file_df$name)
  
  for (i in seq_len(nrow(file_df))) {
    path  <- file_df$datapath[i]
    fname <- file_df$name[i]
    sname <- fallback_names[i]
    tryCatch({
      df <- read_csv_safe(path)
      if (nrow(df)==0) stop("File is empty.")
      dcol <- detect_date_col(df)
      if (is.null(dcol)) stop("No date column detected.")
      df$date <- standardise_dates(df[[dcol]], tz)
      if (dcol != "date") df[[dcol]] <- NULL
      
      # Does this file already carry its own station / site label?
      scol <- detect_station_col(df, date_col="date")
      has_own_labels <- FALSE
      if (!is.null(scol)) {
        vals <- trimws(as.character(df[[scol]]))
        has_own_labels <- any(vals != "" & !is.na(vals))
      }
      
      if (has_own_labels) {
        if (scol != station_col) {
          df[[station_col]] <- df[[scol]]
          if (scol %in% names(df)) df[[scol]] <- NULL
        }
        # blank rows within an otherwise-labelled file fall back to
        # the file-level name (typed, or derived from the filename)
        empty <- is.na(df[[station_col]]) | trimws(df[[station_col]])==""
        df[[station_col]][empty] <- sname
        detected[fname] <- TRUE
      } else {
        df[[station_col]] <- sname
      }
      
      df <- .coerce_numeric(df, skip=c("date", station_col, "source_file"))
      df$source_file <- fname
      parts[[i]] <- df
    }, error=function(e)
      errors <<- c(errors, paste0("[", fname, "/", sname, "] ", e$message)))
  }
  
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts))
    stop(paste0("All files failed:\n", paste(errors, collapse="\n")))
  
  # Reorder: date, station_col, then everything else
  all_cols <- unique(unlist(lapply(parts, names)))
  all_cols <- c("date", station_col,
                setdiff(all_cols, c("date", station_col)))
  parts <- lapply(parts, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop=FALSE]
  })
  
  out <- dplyr::bind_rows(parts)
  out <- out[!is.na(out$date), , drop=FALSE]
  out <- dplyr::arrange(out, .data[[station_col]], date)
  
  if (length(errors)) attr(out, "load_warnings") <- errors
  attr(out, "station_auto_detected") <- detected
  out
}

# ── QC helpers ───────────────────────────────────────────────
station_completeness <- function(df, station_col="station") {
  pc <- setdiff(names(df), c("date", station_col, "source_file"))
  pc <- pc[sapply(df[pc], is.numeric)]
  if (!length(pc) || !station_col %in% names(df)) return(NULL)
  sts <- sort(unique(as.character(df[[station_col]])))
  out <- lapply(sts, function(st) {
    sub <- df[df[[station_col]]==st, pc, drop=FALSE]
    n   <- nrow(sub)
    pct <- sapply(pc, function(v) round(sum(!is.na(sub[[v]]))/max(n,1)*100))
    r   <- as.data.frame(t(pct), stringsAsFactors=FALSE)
    cbind(Station=st, `N rows`=n, r, stringsAsFactors=FALSE)
  })
  dplyr::bind_rows(out)
}

station_summary_stats <- function(df, station_col="station") {
  pc <- setdiff(names(df), c("date", station_col, "source_file"))
  pc <- pc[sapply(df[pc], is.numeric)]
  if (!length(pc) || !station_col %in% names(df)) return(NULL)
  rows <- list()
  for (st in sort(unique(as.character(df[[station_col]])))) {
    for (p in pc) {
      v <- df[[p]][df[[station_col]]==st & !is.na(df[[p]])]
      if (!length(v)) next
      rows[[length(rows)+1]] <- data.frame(
        Station=st, Parameter=p, N=length(v),
        Mean=round(mean(v),3), SD=round(sd(v),3),
        Min=round(min(v),3), Median=round(median(v),3),
        Max=round(max(v),3), stringsAsFactors=FALSE)
    }
  }
  if (!length(rows)) return(NULL)
  dplyr::bind_rows(rows)
}

# Stack-mode QC (no station column)
stack_completeness <- function(df) {
  pc <- setdiff(names(df), c("date","source_file"))
  pc <- pc[sapply(df[pc], is.numeric)]
  if (!length(pc)) return(NULL)
  files <- sort(unique(as.character(df$source_file)))
  out <- lapply(files, function(f) {
    sub <- df[df$source_file==f, pc, drop=FALSE]
    n   <- nrow(sub)
    pct <- sapply(pc, function(v) round(sum(!is.na(sub[[v]]))/max(n,1)*100))
    r   <- as.data.frame(t(pct), stringsAsFactors=FALSE)
    cbind(File=f, `N rows`=n, r, stringsAsFactors=FALSE)
  })
  dplyr::bind_rows(out)
}

stack_summary_stats <- function(df) {
  pc <- setdiff(names(df), c("date","source_file"))
  pc <- pc[sapply(df[pc], is.numeric)]
  if (!length(pc)) return(NULL)
  rows <- list()
  for (p in pc) {
    v <- df[[p]][!is.na(df[[p]])]
    if (!length(v)) next
    rows[[length(rows)+1]] <- data.frame(
      Parameter=p, N=length(v),
      Mean=round(mean(v),3), SD=round(sd(v),3),
      Min=round(min(v),3), Median=round(median(v),3),
      Max=round(max(v),3), stringsAsFactors=FALSE)
  }
  if (!length(rows)) return(NULL)
  dplyr::bind_rows(rows)
}

# ── Station colour palette ────────────────────────────────────
build_station_palette <- function(stations) {
  n   <- length(stations)
  pal <- if (n<=9) RColorBrewer::brewer.pal(max(3,n),"Set1")[seq_len(n)]
  else colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(n)
  setNames(pal, stations)
}

# ── Temporal averaging helper ─────────────────────────────────
.floor_date_by <- function(x, period) {
  switch(period,
         hour  = lubridate::floor_date(x,"hour"),
         day   = as.POSIXct(as.Date(x), tz="UTC"),
         week  = lubridate::floor_date(x,"week"),
         month = lubridate::floor_date(x,"month"),
         year  = lubridate::floor_date(x,"year"),
         as.POSIXct(as.Date(x), tz="UTC"))
}

# ================================================================
#  run_comparative_plot
#  Dispatches 4 custom ggplot2 types + injects type=station_col
#  for all openair plots that accept it.
# ================================================================
run_comparative_plot <- function(pt, df, station_col, p) {
  if (is.null(df)||nrow(df)==0) stop("No data available.")
  if (!station_col %in% names(df))
    stop(paste0("Station column '", station_col, "' not in data."))
  if (!"date" %in% names(df)) stop("Data must have a 'date' column.")
  if (!inherits(df$date,"POSIXct"))
    df$date <- suppressWarnings(as.POSIXct(df$date, tz="UTC"))
  
  stations   <- sort(unique(as.character(df[[station_col]])))
  pc <- setdiff(names(df), c("date", station_col, "source_file"))
  pc <- pc[sapply(df[pc], is.numeric)]
  pal        <- build_station_palette(stations)
  avg_time   <- p$avg_time %||% "day"
  
  base_theme <- ggplot2::theme_bw(base_size=11) +
    ggplot2::theme(
      legend.position  = "bottom",
      strip.background = ggplot2::element_rect(fill="#006064"),
      strip.text       = ggplot2::element_text(colour="white",face="bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face="bold",colour="#006064",size=12),
      plot.subtitle    = ggplot2::element_text(colour="#555555",size=10)
    )
  
  # ── openair plots: inject type=station_col ──────────────────
  openair_type_ok <- c("timePlot","timeVariation","calendarPlot",
                       "smoothTrend","TheilSen","trendLevel",
                       "scatterPlot","windRose","pollutionRose",
                       "percentileRose","polarPlot","polarAnnulus",
                       "summaryPlot")
  if (pt %in% openair_type_ok) {
    df[[station_col]] <- as.character(df[[station_col]])
    pmod <- p
    if (pt != "summaryPlot") pmod$type <- station_col
    return(run_openair_plot(pt, df, pmod))
  }
  
  polls_multi <- if (!is.null(p$pollutant) && length(p$pollutant)>0)
    intersect(p$pollutant, pc) else pc[seq_len(min(4,length(pc)))]
  poll_single <- if (!is.null(p$pollutant) && length(p$pollutant)>0)
    intersect(p$pollutant, pc)[1] else pc[1]
  
  switch(pt,
         
         # ── Multi-line time series ──────────────────────────────
         station_timeseries = {
           if (!length(polls_multi)) stop("No valid parameters selected.")
           df$period_ <- .floor_date_by(df$date, avg_time)
           agg <- dplyr::group_by(df, .data[[station_col]], period_) |>
             dplyr::summarise(dplyr::across(dplyr::all_of(polls_multi),
                                            ~mean(.x,na.rm=TRUE)), .groups="drop") |>
             dplyr::rename(date=period_)
           long <- tidyr::pivot_longer(agg, dplyr::all_of(polls_multi),
                                       names_to="parameter", values_to="value")
           ggplot2::ggplot(long,
                           ggplot2::aes(x=date, y=value,
                                        colour=.data[[station_col]],
                                        group =.data[[station_col]])) +
             ggplot2::geom_line(linewidth=0.55, na.rm=TRUE) +
             ggplot2::facet_wrap(~parameter, scales="free_y", ncol=1) +
             ggplot2::scale_colour_manual(values=pal, name="Station") +
             ggplot2::scale_x_datetime(date_labels="%b %Y") +
             ggplot2::labs(
               title    = "Station Comparison — Time Series",
               subtitle = paste0("Averaged to: ", avg_time),
               x=NULL, y="Value") +
             base_theme
         },
         
         # ── Box plots per station ───────────────────────────────
         boxplot_station = {
           if (!length(polls_multi)) stop("No valid parameters selected.")
           sub <- df[,c(station_col, polls_multi), drop=FALSE]
           long <- tidyr::pivot_longer(sub, dplyr::all_of(polls_multi),
                                       names_to="parameter", values_to="value")
           long[[station_col]] <- factor(long[[station_col]], levels=stations)
           ggplot2::ggplot(long,
                           ggplot2::aes(x=.data[[station_col]], y=value,
                                        fill=.data[[station_col]])) +
             ggplot2::geom_boxplot(outlier.size=.7, outlier.alpha=.35,
                                   notch=FALSE) +
             ggplot2::facet_wrap(~parameter, scales="free_y") +
             ggplot2::scale_fill_manual(values=pal, guide="none") +
             ggplot2::labs(
               title="Station Comparison — Distributions",
               x="Station", y="Value") +
             ggplot2::theme(
               axis.text.x=ggplot2::element_text(angle=30,hjust=1,size=9)) +
             base_theme
         },
         
         # ── Mean bar chart ──────────────────────────────────────
         mean_bar = {
           if (!length(polls_multi)) stop("No valid parameters selected.")
           agg <- dplyr::group_by(df, .data[[station_col]]) |>
             dplyr::summarise(dplyr::across(dplyr::all_of(polls_multi),
                                            ~mean(.x,na.rm=TRUE)), .groups="drop")
           long <- tidyr::pivot_longer(agg, dplyr::all_of(polls_multi),
                                       names_to="parameter", values_to="mean_val")
           long[[station_col]] <- factor(long[[station_col]], levels=stations)
           ggplot2::ggplot(long,
                           ggplot2::aes(x=.data[[station_col]], y=mean_val,
                                        fill=.data[[station_col]])) +
             ggplot2::geom_col(width=0.65) +
             ggplot2::geom_text(ggplot2::aes(label=round(mean_val,1)),
                                vjust=-.3, size=3, fontface="bold") +
             ggplot2::facet_wrap(~parameter, scales="free_y") +
             ggplot2::scale_fill_manual(values=pal, guide="none") +
             ggplot2::labs(title="Station Comparison — Mean Concentrations",
                           x="Station", y="Mean Value") +
             ggplot2::theme(
               axis.text.x=ggplot2::element_text(angle=30,hjust=1,size=9)) +
             base_theme
         },
         
         # ── Inter-station correlation heat-map ─────────────────
         correlation_heatmap = {
           if (is.null(poll_single)||!poll_single %in% pc)
             stop("Select a valid pollutant for the correlation heat-map.")
           wide <- tidyr::pivot_wider(
             df[,c("date", station_col, poll_single)],
             names_from=station_col, values_from=poll_single, values_fn=mean)
           wide$date <- lubridate::floor_date(wide$date,"hour")
           wide <- dplyr::group_by(wide, date) |>
             dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                            ~mean(.x,na.rm=TRUE)), .groups="drop")
           mat_cols <- intersect(stations, names(wide))
           if (length(mat_cols)<2) stop("Need at least 2 stations for correlation.")
           mat   <- cor(wide[,mat_cols,drop=FALSE], use="pairwise.complete.obs")
           mat[is.na(mat)] <- 0
           mdf   <- as.data.frame(as.table(mat))
           names(mdf) <- c("Station1","Station2","r")
           mdf$label  <- round(mdf$r, 2)
           mdf$Station1 <- factor(mdf$Station1, levels=mat_cols)
           mdf$Station2 <- factor(mdf$Station2, levels=rev(mat_cols))
           ggplot2::ggplot(mdf,
                           ggplot2::aes(x=Station1, y=Station2, fill=r)) +
             ggplot2::geom_tile(colour="white", linewidth=.5) +
             ggplot2::geom_text(ggplot2::aes(label=label),
                                size=3.5, colour="white", fontface="bold") +
             ggplot2::scale_fill_gradient2(
               low="#1A237E", mid="#ECEFF1", high="#006064",
               midpoint=0, limits=c(-1,1), name="Pearson r") +
             ggplot2::labs(
               title    = paste0("Inter-station Correlation  —  ", poll_single),
               subtitle = "Pearson r  (pairwise complete observations)",
               x=NULL, y=NULL) +
             ggplot2::theme_minimal(base_size=11) +
             ggplot2::theme(
               axis.text.x=ggplot2::element_text(angle=30,hjust=1),
               plot.title=ggplot2::element_text(face="bold",colour="#006064"),
               panel.grid=ggplot2::element_blank())
         },
         
         stop(paste0("Unknown comparative plot type: '", pt, "'"))
  )
}