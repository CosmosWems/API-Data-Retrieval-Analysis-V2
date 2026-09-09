# ================================================================
#  R/api_helpers.R  –  OpenAQ v3 REST API wrappers
#
#  Rate-limit implementation per https://docs.openaq.org/using-the-api/rate-limits
#  Free tier: 60 req/min  ·  2,000 req/hour
#
#  Headers parsed from every response:
#    x-ratelimit-used      – requests used in current window
#    x-ratelimit-limit     – max requests in window
#    x-ratelimit-remaining – requests left before 429
#    x-ratelimit-reset     – seconds until the window resets
#
#  Guards:
#   • rl_state environment holds the last-known header values
#   • rate_limit_guard() sleeps before a call when remaining < threshold
#   • After a 429, sleeps for x-ratelimit-reset seconds before retry
#   • rl_gauge_html() returns an HTML snippet for the UI gauge
# ================================================================

OPENAQ_BASE    <- "https://api.openaq.org/v3"
OPENAQ_MAX_RAD <- 25000L   # metres – API hard limit (25 km)

# ── rate-limit state (shared across calls in one session) ─────
rl_state <- new.env(parent = emptyenv())
rl_state$used      <- 0L
rl_state$limit     <- 60L    # default free-tier per-minute limit
rl_state$remaining <- 60L
rl_state$reset_sec <- 60L
rl_state$last_call <- Sys.time()
rl_state$window    <- "minute"  # "minute" or "hour"

# Threshold: pause when fewer than this many requests remain
RL_PAUSE_THRESHOLD <- 5L
RL_PAUSE_SLEEP     <- 2    # seconds to sleep when near limit

# ── Parse rate-limit headers from httr response ───────────────
.parse_rl_headers <- function(resp) {
  h <- httr::headers(resp)
  used      <- suppressWarnings(as.integer(h[["x-ratelimit-used"]]))
  limit     <- suppressWarnings(as.integer(h[["x-ratelimit-limit"]]))
  remaining <- suppressWarnings(as.integer(h[["x-ratelimit-remaining"]]))
  reset_sec <- suppressWarnings(as.numeric(h[["x-ratelimit-reset"]]))

  if (!is.na(used))      rl_state$used      <- used
  if (!is.na(limit))     rl_state$limit     <- limit
  if (!is.na(remaining)) rl_state$remaining <- remaining
  if (!is.na(reset_sec)) rl_state$reset_sec <- reset_sec
  rl_state$last_call <- Sys.time()

  invisible(list(used=used, limit=limit,
                 remaining=remaining, reset_sec=reset_sec))
}

# ── Guard: called BEFORE each API request ────────────────────
rate_limit_guard <- function(n_planned = 1L) {
  rem <- rl_state$remaining %||% 60L
  if (is.na(rem)) return(invisible(NULL))

  if (rem <= RL_PAUSE_THRESHOLD) {
    wait <- max(rl_state$reset_sec %||% RL_PAUSE_SLEEP,
                RL_PAUSE_SLEEP)
    message(sprintf(
      "[Rate-limit] Only %d requests remaining. Pausing %.0f s for window reset.",
      rem, wait))
    Sys.sleep(wait)
  }
  invisible(NULL)
}

# ── HTML gauge for sidebar / fetch panel ─────────────────────
#' @return character HTML safe to wrap in shiny::HTML()
rl_gauge_html <- function() {
  used  <- rl_state$used      %||% 0L
  lim   <- rl_state$limit     %||% 60L
  rem   <- rl_state$remaining %||% 60L
  rst   <- rl_state$reset_sec %||% 60L

  if (is.na(lim) || lim == 0L) {
    return('<div class="rl-gauge rl-ok"><span>Rate limit: no data yet</span></div>')
  }

  pct   <- round(used / lim * 100)
  col   <- if (pct >= 90) "#ff1744"
           else if (pct >= 70) "#ffc107"
           else "#00ff87"

  cls   <- if (pct >= 90) "rl-danger"
           else if (pct >= 70) "rl-warn"
           else "rl-ok"

  sprintf(
    '<div class="rl-gauge %s">
       <div class="rl-row">
         <span class="rl-label">API Requests</span>
         <span class="rl-nums">%d / %d used</span>
       </div>
       <div class="rl-bar-bg">
         <div class="rl-bar-fill" style="width:%d%%;background:%s;"></div>
       </div>
       <div class="rl-row" style="margin-top:4px;">
         <span class="rl-sub">%d remaining</span>
         <span class="rl-sub">resets in ~%.0fs</span>
       </div>
     </div>',
    cls, used, lim, min(pct, 100), col, rem, rst
  )
}

# ── Core GET ─────────────────────────────────────────────────
openaq_get <- function(endpoint, params = list(), api_key) {

  rate_limit_guard()

  resp <- httr::GET(
    paste0(OPENAQ_BASE, endpoint),
    httr::add_headers("X-API-Key" = api_key,
                      "Accept"    = "application/json"),
    query = if (length(params) > 0) params else NULL
  )

  .parse_rl_headers(resp)

  sc   <- httr::status_code(resp)
  body <- httr::content(resp, "text", encoding = "UTF-8")

  if (sc == 401) stop("Invalid API key (401). Verify at explore.openaq.org.")
  if (sc == 403) stop("Access forbidden (403). Check API key permissions.")
  if (sc == 404) stop("Endpoint not found (404).")
  if (sc == 422) stop(paste0("Validation error (422): ", body))
  if (sc == 429) {
    wait <- max(rl_state$reset_sec %||% 60, 5)
    msg  <- sprintf(
      "Rate limit exceeded (429). Waiting %.0f seconds then retrying once.", wait)
    message(msg)
    Sys.sleep(wait)
    # one automatic retry
    rate_limit_guard()
    resp2 <- httr::GET(
      paste0(OPENAQ_BASE, endpoint),
      httr::add_headers("X-API-Key" = api_key,
                        "Accept"    = "application/json"),
      query = if (length(params) > 0) params else NULL
    )
    .parse_rl_headers(resp2)
    sc2   <- httr::status_code(resp2)
    body  <- httr::content(resp2, "text", encoding = "UTF-8")
    if (sc2 == 429)
      stop(sprintf(
        "Rate limit still exceeded (429) after %.0f s wait. \n  Limit: %d/window  Used: %d  Remaining: %d  Resets in: %.0f s.\n  Wait for the window to reset before retrying.",
        wait, rl_state$limit, rl_state$used,
        rl_state$remaining, rl_state$reset_sec))
    if (sc2 != 200)
      stop(paste0("API error after retry (", sc2, "): ", body))
    sc   <- sc2
  }
  if (sc >= 500) stop(paste0("OpenAQ server error (", sc, "). Try later."))
  if (sc != 200) stop(paste0("API error (", sc, "): ", body))

  tryCatch({
    jsonlite::fromJSON(txt = body,
                       simplifyVector    = TRUE,
                       simplifyDataFrame = TRUE,
                       flatten           = TRUE)
  }, error = function(e) {
    message("[openaq_get] JSON parse failed: ", e$message)
    list(results = data.frame(), meta = list(error = e$message))
  })
}

# ── Key test ─────────────────────────────────────────────────
test_api_key <- function(api_key) {
  tryCatch({
    openaq_get("/countries", list(limit = 1), api_key)
    list(ok = TRUE, msg = "Connected.")
  }, error = function(e) list(ok = FALSE, msg = as.character(e$message)))
}

# ── Countries ────────────────────────────────────────────────
get_countries <- function(api_key, limit = 300) {
  res <- openaq_get("/countries", list(limit = limit), api_key)
  if (is.null(res$results) || length(res$results) == 0)
    return(data.frame(id=integer(0), name=character(0),
                      code=character(0), stringsAsFactors=FALSE))
  as.data.frame(res$results, stringsAsFactors = FALSE)
}

# ── Location searches ────────────────────────────────────────
search_locations_by_name <- function(name, limit=50, param_id=NULL, api_key) {
  p <- list(search=trimws(name), limit=as.integer(limit))
  if (!is.null(param_id) && nchar(as.character(param_id))>0)
    p$parameters_id <- as.integer(param_id)
  res <- openaq_get("/locations", p, api_key)
  if (is.null(res$results)||length(res$results)==0) return(NULL)
  res$results
}

search_locations_by_coords <- function(lat, lon, radius_km=25,
                                        limit=100, api_key) {
  lat    <- as.numeric(lat); lon <- as.numeric(lon)
  if (!is.finite(lat)||!is.finite(lon))
    stop("Latitude and longitude must be valid numbers.")
  radius <- min(as.integer(round(as.numeric(radius_km)*1000)),
                OPENAQ_MAX_RAD)
  res <- openaq_get("/locations",
    list(coordinates=paste0(lat,",",lon), radius=radius,
         limit=as.integer(limit)), api_key)
  if (is.null(res$results)||length(res$results)==0) return(NULL)
  res$results
}

search_locations_by_bbox <- function(min_lon,min_lat,max_lon,max_lat,
                                      limit=200, api_key) {
  vals <- as.numeric(c(min_lon,min_lat,max_lon,max_lat))
  if (any(!is.finite(vals))) stop("All bounding box values must be valid numbers.")
  if (vals[1]>=vals[3]) stop("Min longitude must be less than max.")
  if (vals[2]>=vals[4]) stop("Min latitude must be less than max.")
  res <- openaq_get("/locations",
    list(bbox=paste(vals,collapse=","), limit=as.integer(limit)), api_key)
  if (is.null(res$results)||length(res$results)==0) return(NULL)
  res$results
}

search_locations_by_country <- function(country_id, limit=200, api_key) {
  res <- openaq_get("/locations",
    list(countries_id=as.integer(country_id), limit=as.integer(limit)),
    api_key)
  if (is.null(res$results)||length(res$results)==0) return(NULL)
  res$results
}

# ── Sensors for a location ───────────────────────────────────
get_location_sensors <- function(location_id, api_key) {
  res <- openaq_get(
    endpoint = paste0("/locations/", as.integer(location_id), "/sensors"),
    params   = list(),
    api_key  = api_key)
  if (is.null(res$results)||length(res$results)==0) return(data.frame())
  as.data.frame(res$results, stringsAsFactors=FALSE)
}

# ── Measurements ─────────────────────────────────────────────
.fmt_dt <- function(x) format(as.POSIXct(x, tz="UTC"), "%Y-%m-%dT%H:%M:%SZ")

fetch_measurements <- function(sensor_id, date_from, date_to,
                                agg="hourly", api_key, limit=1000) {
  ep <- switch(agg,
    raw    = paste0("/sensors/", sensor_id, "/measurements"),
    hourly = paste0("/sensors/", sensor_id, "/hours"),
    daily  = paste0("/sensors/", sensor_id, "/days"),
    stop("agg must be 'raw', 'hourly', or 'daily'"))
  res <- openaq_get(ep,
    list(datetime_from=.fmt_dt(date_from),
         datetime_to=.fmt_dt(date_to),
         limit=as.integer(limit)), api_key)
  r <- res$results
  if (is.null(r)) return(NULL)
  if (is.list(r) && !is.data.frame(r) && length(r)==0) return(NULL)
  if (is.data.frame(r) && nrow(r)==0) return(NULL)
  tryCatch(as.data.frame(r, stringsAsFactors=FALSE),
           error=function(e){ message("[fetch_measurements] ", e$message); NULL })
}
