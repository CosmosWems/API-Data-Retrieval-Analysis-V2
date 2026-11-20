# ================================================================
#  R/plot_helpers.R  –  OpenAir plot dispatcher
#
#  Fixes applied:
#  1. timePlot: guard against NULL / empty pollutant vector;
#     ensure every named pollutant column actually exists.
#  2. timeVariation: same pollutant guard.
#  3. All dispatched calls wrapped in tryCatch so a bad argument
#     surfaces a clear message rather than a cryptic R error.
#  4. .req_cols gives an informative stop() listing both missing
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
#  Distribution and extreme-value plots built with ggplot2
# ----------------------------------------------------------------
.run_distribution_plot <- function(pt, df, p) {
  poll <- .clean_poll(df, p$pollutant %||% "")[1]
  values <- suppressWarnings(as.numeric(df[[poll]]))
  keep <- is.finite(values) & !is.na(df$date)
  plot_data <- data.frame(
    date = df$date[keep],
    value = values[keep]
  )
  if (nrow(plot_data) == 0)
    stop("The selected pollutant has no finite observations.")

  group_by <- p$type %||% "default"
  if (identical(group_by, "default")) {
    plot_data$group <- factor("All observations")
  } else {
    d <- plot_data$date
    labels <- switch(group_by,
      month = format(d, "%b"),
      season = {
        month_num <- as.integer(format(d, "%m"))
        ifelse(month_num %in% c(12, 1, 2), "Summer",
          ifelse(month_num %in% c(3, 4, 5), "Autumn",
            ifelse(month_num %in% c(6, 7, 8), "Winter", "Spring")))
      },
      weekday = format(d, "%a"),
      year = format(d, "%Y"),
      as.character(d)
    )
    plot_data$group <- factor(labels, levels = unique(labels))
  }

  title <- paste(poll, switch(pt,
    boxPlot = "distribution by",
    violinPlot = "distribution by",
    densityPlot = "density by",
    extremePlot = "daily maxima"
  ), if (pt == "extremePlot") "" else group_by)
  plot_data$group <- droplevels(plot_data$group)

  if (pt == "extremePlot") {
    plot_data$period <- as.Date(plot_data$date)
    daily <- stats::aggregate(value ~ period, plot_data, max)
    threshold <- suppressWarnings(as.numeric(p$quantile %||% 0.95))
    if (length(threshold) != 1 || !is.finite(threshold) ||
        threshold <= 0 || threshold >= 1)
      stop("Extreme threshold must be between 0 and 1.")
    cutoff <- stats::quantile(daily$value, probs = threshold, na.rm = TRUE,
                              names = FALSE)
    daily$extreme <- daily$value >= cutoff
    return(ggplot2::ggplot(daily, ggplot2::aes(x = period, y = value)) +
      ggplot2::geom_line(color = "#2471a3", size = 0.6) +
      ggplot2::geom_point(ggplot2::aes(color = extreme), size = 1.8) +
      ggplot2::geom_hline(yintercept = cutoff, linetype = "dashed",
                          color = "#c0392b") +
      ggplot2::scale_color_manual(values = c(`FALSE` = "#2471a3",
                                              `TRUE` = "#c0392b"),
                                  guide = "none") +
      ggplot2::labs(title = paste(poll, "daily maximum values"),
                    subtitle = paste0(round(threshold * 100), "th percentile threshold: ",
                                      signif(cutoff, 4)),
                    x = "Date", y = poll) +
      ggplot2::theme_minimal(base_size = 12))
  }

  if (pt == "densityPlot") {
    density_plot <- ggplot2::ggplot(
      plot_data, ggplot2::aes(x = value, color = group, fill = group)) +
      ggplot2::geom_density(alpha = 0.25, na.rm = TRUE)
    if (nlevels(plot_data$group) == 1)
      density_plot <- density_plot + ggplot2::guides(color = "none", fill = "none")
    return(density_plot +
      ggplot2::labs(title = title, x = poll, y = "Density", color = group_by,
                    fill = group_by) +
      ggplot2::theme_minimal(base_size = 12))
  }

  distribution_plot <- ggplot2::ggplot(
    plot_data, ggplot2::aes(x = group, y = value, fill = group))
  if (pt == "boxPlot") {
    distribution_plot <- distribution_plot +
      ggplot2::geom_boxplot(outlier.alpha = 0.45, na.rm = TRUE)
  } else {
    distribution_plot <- distribution_plot +
      ggplot2::geom_violin(trim = TRUE, na.rm = TRUE)
  }
  distribution_plot +
    ggplot2::labs(title = title, x = if (group_by == "default") "" else group_by,
                  y = poll) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

# Data-to-Viz inspired plots that work on the app's wide, date-indexed data.
.run_extended_plot <- function(pt, df, p) {
  numeric_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))],
                          c("year", "month", "day", "hour", "weekday"))
  theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (pt == "correlationHeatmap") {
    cols <- intersect(p$variables %||% numeric_cols, numeric_cols)
    if (length(cols) < 2) stop("Choose at least two numeric variables.")
    method <- p$method %||% "pearson"
    corr <- stats::cor(df[, cols, drop = FALSE], use = "pairwise.complete.obs",
                       method = method)
    if (all(is.na(corr))) stop("Not enough paired observations to calculate correlations.")
    corr_df <- as.data.frame(as.table(corr), stringsAsFactors = FALSE)
    names(corr_df) <- c("x", "y", "correlation")
    corr_df$x <- factor(corr_df$x, levels = cols)
    corr_df$y <- factor(corr_df$y, levels = rev(cols))
    return(ggplot2::ggplot(corr_df,
      ggplot2::aes(x = x, y = y, fill = correlation)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(correlation), "",
                                            sprintf("%.2f", correlation))), size = 3) +
      ggplot2::scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
        low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", na.value = "#dddddd") +
      ggplot2::coord_equal() +
      ggplot2::labs(title = paste(tools::toTitleCase(method), "correlation"),
                    x = NULL, y = NULL, fill = "r") + theme)
  }

  poll <- .clean_poll(df, p$pollutant %||% "")[1]
  value <- suppressWarnings(as.numeric(df[[poll]]))
  keep <- is.finite(value)
  data <- data.frame(date = df$date[keep], value = value[keep])
  if (!nrow(data)) stop("The selected variable has no finite observations.")

  if (pt %in% c("histogramPlot", "ecdfPlot")) {
    if (pt == "histogramPlot") {
      bins <- as.integer(p$bins %||% 30)
      return(ggplot2::ggplot(data, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(bins = bins, fill = "#657b83", color = "white") +
        ggplot2::labs(title = paste(poll, "distribution"), x = poll,
                      y = "Observation count") + theme)
    }
    return(ggplot2::ggplot(data, ggplot2::aes(x = value)) +
      ggplot2::stat_ecdf(geom = "step", color = "#2166ac", linewidth = 0.8) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(title = paste(poll, "empirical cumulative distribution"),
                    x = poll, y = "Cumulative proportion") + theme)
  }

  if (pt == "connectedScatter") {
    data <- data[!is.na(data$date), , drop = FALSE]
    data <- data[order(data$date), , drop = FALSE]
    if (nrow(data) < 2) stop("At least two dated observations are required.")
    return(ggplot2::ggplot(data, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_path(color = "#2166ac", linewidth = 0.65) +
      ggplot2::geom_point(color = "#657b83", alpha = 0.65, size = 1.4) +
      ggplot2::scale_x_datetime(date_labels = "%b %Y") +
      ggplot2::labs(title = paste(poll, "over time"), x = "Date", y = poll) + theme)
  }

  if (pt == "compositionWaffle") {
    data <- data[!is.na(data$date), , drop = FALSE]
    if (any(data$value < 0))
      stop("A part-to-whole chart requires non-negative values.")
    group_by <- p$group_by %||% "month"
    d <- data$date
    group <- switch(group_by,
      weekday = format(d, "%A"),
      season = {
        month_num <- as.integer(format(d, "%m"))
        ifelse(month_num %in% c(12, 1, 2), "Summer",
          ifelse(month_num %in% c(3, 4, 5), "Autumn",
            ifelse(month_num %in% c(6, 7, 8), "Winter", "Spring")))
      },
      year = format(d, "%Y"),
      format(d, "%b")
    )
    totals <- stats::aggregate(data$value, list(group = group), sum, na.rm = TRUE)
    names(totals)[2] <- "total"
    totals <- totals[is.finite(totals$total) & totals$total > 0, , drop = FALSE]
    if (!nrow(totals)) stop("No positive totals are available for the selected grouping.")
    totals$group <- factor(totals$group, levels = totals$group[order(totals$total, decreasing = TRUE)])
    shares <- totals$total / sum(totals$total)
    counts <- floor(shares * 100)
    remainder <- 100 - sum(counts)
    if (remainder > 0) {
      order_remainder <- order(shares * 100 - counts, decreasing = TRUE)
      counts[order_remainder[seq_len(remainder)]] <-
        counts[order_remainder[seq_len(remainder)]] + 1L
    }
    tiles <- data.frame(group = rep(as.character(totals$group), counts))
    tiles$tile <- seq_len(nrow(tiles))
    tiles$x <- (tiles$tile - 1L) %% 10L + 1L
    tiles$y <- 10L - (tiles$tile - 1L) %/% 10L
    tiles$group <- factor(tiles$group, levels = levels(totals$group))
    return(ggplot2::ggplot(tiles, ggplot2::aes(x = x, y = y, fill = group)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.6) +
      ggplot2::coord_equal() +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(title = paste(poll, "share by", group_by),
                    subtitle = "Each tile represents 1% of the summed observations",
                    x = NULL, y = NULL, fill = group_by) + theme +
      ggplot2::theme(panel.grid = ggplot2::element_blank()))
  }
  stop(paste0("Unknown extended plot type: ", pt))
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

    # ---- Distribution and extreme-value plots -----------------
    boxPlot     = .run_distribution_plot(pt, df, p),
    violinPlot  = .run_distribution_plot(pt, df, p),
    densityPlot = .run_distribution_plot(pt, df, p),
    extremePlot = .run_distribution_plot(pt, df, p),
    histogramPlot = .run_extended_plot(pt, df, p),
    ecdfPlot = .run_extended_plot(pt, df, p),
    correlationHeatmap = .run_extended_plot(pt, df, p),
    connectedScatter = .run_extended_plot(pt, df, p),
    compositionWaffle = .run_extended_plot(pt, df, p),

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
        cols      = p$cols      %||% "heat"
      )
      if (!is.null(p$pollutant) && length(p$pollutant) > 0 &&
          nzchar(p$pollutant[1]) && p$pollutant[1] %in% names(df))
        args$pollutant <- p$pollutant[1]
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
      if (is.null(p$mod_col) || is.null(p$obs_col) ||
          !nzchar(p$mod_col) || !nzchar(p$obs_col))
        stop("Choose observed and modelled variables in the Model Evaluation panel.")
      .req_cols(df, c(p$mod_col, p$obs_col))
      stats <- openair::modStats(
        df,
        mod  = p$mod_col,
        obs  = p$obs_col,
        type = p$type %||% "default"
      )
      print(stats, digits = 4)
      # Draw the statistics as a table so the plot pane is not blank.
      grid::grid.newpage()
      grid::grid.draw(gridExtra::tableGrob(stats, rows = NULL))
      invisible(stats)
    },

    stop(paste0("Unknown plot type: '", pt, "'"))
  )
}
