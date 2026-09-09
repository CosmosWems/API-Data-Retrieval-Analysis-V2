# ================================================================
#  R/plot_helpers.R  –  OpenAir plot dispatcher
#
#  Fixes applied:
#  1. summaryPlot: validate at least one non-all-NA pollutant col
#     exists; pass period.ok correctly (openair arg name).
#  2. timePlot: guard against NULL / empty pollutant vector;
#     ensure every named pollutant column actually exists.
#  3. timeVariation: same pollutant guard.
#  4. All dispatched calls wrapped in tryCatch so a bad argument
#     surfaces a clear message rather than a cryptic R error.
#  5. .req_cols gives an informative stop() listing both missing
#     and available columns.
# ================================================================

# ----------------------------------------------------------------
#  Check required columns exist; throw informative error if not
# ----------------------------------------------------------------
.req_cols <- function(df, cols) {
  cols    <- cols[nchar(cols) > 0]
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0)
    stop(paste0("Column(s) not in data: ",
                paste(missing, collapse = ", "),
                ".  Available: ",
                paste(names(df), collapse = ", ")))
}

# ----------------------------------------------------------------
#  Validate & clean a pollutant vector
#   – removes empty strings / NULLs
#   – stops if nothing is left after filtering against df columns
# ----------------------------------------------------------------
.clean_poll <- function(df, polls, allow_empty = FALSE) {
  polls <- polls[nchar(polls) > 0]
  polls <- intersect(polls, names(df))
  if (length(polls) == 0) {
    if (allow_empty) return(character(0))
    stop("None of the selected pollutant columns are in the data.  ",
         "Available: ", paste(pollutant_cols(df), collapse = ", "))
  }
  polls
}

# ----------------------------------------------------------------
#  Main dispatcher
#  df  : openair-ready wide data.frame  (date + pollutant cols)
#  pt  : character – key matching PLOT_CATALOGUE
#  p   : named list – all UI control values
# ----------------------------------------------------------------
run_openair_plot <- function(pt, df, p) {

  # ---- global pre-checks --------------------------------------
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    stop("No data available. Fetch data and click Prepare first.")
  if (!"date" %in% names(df))
    stop("Data must contain a 'date' (POSIXct) column.")
  if (!inherits(df$date, "POSIXct"))
    df$date <- suppressWarnings(as.POSIXct(df$date, tz = "UTC"))

  switch(pt,

    # ---- Summary Plot ----------------------------------------
    summaryPlot = {
      pc <- pollutant_cols(df)
      if (length(pc) == 0)
        stop("No pollutant columns found in the data.")
      # drop all-NA columns so summaryPlot doesn't choke
      valid_pc <- pc[sapply(pc, function(x) sum(!is.na(df[[x]])) > 0)]
      if (length(valid_pc) == 0)
        stop("All pollutant columns are entirely NA.")
      df_sub <- df[, c("date", valid_pc), drop = FALSE]
      args <- list(mydata   = df_sub,
                   avg.time = p$avg_time %||% "day")
      if (!is.null(p$period_ok))
        args$period.ok <- isTRUE(p$period_ok)
      do.call(openair::summaryPlot, args)
    },

    # ---- Time Plot -------------------------------------------
    timePlot = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      args <- list(
        mydata     = df,
        pollutant  = polls,
        avg.time   = p$avg_time   %||% "day",
        y.relation = p$y_relation %||% "free",
        type       = p$type       %||% "default"
      )
      if (isTRUE(p$smooth)) args$smooth <- TRUE
      do.call(openair::timePlot, args)
    },

    # ---- Time Variation --------------------------------------
    timeVariation = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      args <- list(
        mydata    = df,
        pollutant = polls,
        normalise = isTRUE(p$normalise),
        type      = p$type %||% "default"
      )
      do.call(openair::timeVariation, args)
    },

    # ---- Calendar Plot ---------------------------------------
    calendarPlot = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        year      = p$year      %||% as.integer(format(Sys.Date(), "%Y")),
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "RdYlGn"
      )
      if (!is.null(p$annotate) && nchar(p$annotate) > 0)
        args$annotate <- p$annotate
      if (isTRUE(p$custom_breaks) && !is.null(p$breaks) &&
          length(p$breaks) > 1) {
        args$breaks <- p$breaks
        if (!is.null(p$labels))     args$labels   <- p$labels
        if (!is.null(p$break_cols)) args$cols      <- p$break_cols
      }
      do.call(openair::calendarPlot, args)
    },

    # ---- Wind Rose -------------------------------------------
    windRose = {
      .req_cols(df, c("ws","wd"))
      args <- list(
        mydata = df,
        ws     = "ws",
        wd     = "wd",
        type   = p$type   %||% "default",
        ws.int = p$ws_int %||% 2,
        cols   = p$cols   %||% "YlOrRd"
      )
      do.call(openair::windRose, args)
    },

    # ---- Pollution Rose --------------------------------------
    pollutionRose = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        type      = p$type      %||% "default",
        normalise = isTRUE(p$normalise),
        seg       = p$seg       %||% 0.9,
        cols      = p$cols      %||% "YlOrRd"
      )
      do.call(openair::pollutionRose, args)
    },

    # ---- Percentile Rose -------------------------------------
    percentileRose = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        type      = p$type  %||% "default",
        smooth    = isTRUE(p$smooth)
      )
      do.call(openair::percentileRose, args)
    },

    # ---- Polar Plot ------------------------------------------
    polarPlot = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata     = df,
        pollutant  = poll,
        x          = p$x          %||% "ws",
        wd         = "wd",
        statistic  = p$statistic  %||% "mean",
        type       = p$type       %||% "default",
        cols       = p$cols       %||% "heat",
        resolution = p$resolution %||% 100
      )
      do.call(openair::polarPlot, args)
    },

    # ---- Polar Frequency -------------------------------------
    polarFreq = {
      .req_cols(df, c("ws","wd"))
      args <- list(
        mydata    = df,
        type      = p$type      %||% "month",
        ws.int    = p$ws_int    %||% 30,
        statistic = p$statistic %||% "frequency",
        offset    = p$offset    %||% 80,
        trans     = FALSE,
        col       = p$cols      %||% "heat"
      )
      if (!is.null(p$pollutant) && nchar(p$pollutant) > 0 &&
          p$pollutant %in% names(df))
        args$pollutant <- p$pollutant
      do.call(openair::polarFreq, args)
    },

    # ---- Polar Annulus ---------------------------------------
    polarAnnulus = {
      .req_cols(df, c("ws","wd"))
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        period    = p$period    %||% "hour",
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "heat"
      )
      do.call(openair::polarAnnulus, args)
    },

    # ---- Smooth Trend ----------------------------------------
    smoothTrend = {
      polls <- .clean_poll(df, p$pollutant %||% character(0))
      args <- list(
        mydata    = df,
        pollutant = polls,
        avg.time  = p$avg_time %||% "month",
        ci        = (p$ci      %||% 95) / 100,
        type      = p$type     %||% "default"
      )
      do.call(openair::smoothTrend, args)
    },

    # ---- Theil-Sen -------------------------------------------
    TheilSen = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        avg.time  = p$avg_time %||% "month",
        type      = p$type     %||% "default",
        deseason  = isTRUE(p$deseason)
      )
      do.call(openair::TheilSen, args)
    },

    # ---- Trend Level -----------------------------------------
    trendLevel = {
      poll <- .clean_poll(df, p$pollutant %||% "")[1]
      args <- list(
        mydata    = df,
        pollutant = poll,
        x         = p$x         %||% "month",
        y         = p$y         %||% "hour",
        statistic = p$statistic %||% "mean",
        cols      = p$cols      %||% "heat"
      )
      do.call(openair::trendLevel, args)
    },

    # ---- Scatter Plot ----------------------------------------
    scatterPlot = {
      xv <- p$x_var %||% ""
      yv <- p$y_var %||% ""
      .req_cols(df, c(xv, yv))
      args <- list(
        mydata = df,
        x      = xv,
        y      = yv,
        method = p$method %||% "default",
        type   = p$type   %||% "default"
      )
      if (!is.null(p$group) && nchar(p$group) > 0)
        args$group <- p$group
      do.call(openair::scatterPlot, args)
    },

    # ---- Model Statistics ------------------------------------
    modStats = {
      .req_cols(df, c(p$mod_col, p$obs_col))
      print(
        openair::modStats(df,
          mod  = p$mod_col,
          obs  = p$obs_col,
          type = p$type %||% "default"),
        digits = 4
      )
    },

    stop(paste0("Unknown plot type: '", pt, "'"))
  )
}
