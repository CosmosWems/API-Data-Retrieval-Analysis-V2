# ================================================================
#  run_app.R  –  API Data Retrieval & Analysis
#
#  Usage (from the openaq_app/ folder):
#    Rscript run_app.R
#    Rscript run_app.R --port 4040 --host 0.0.0.0
#
#  Or from RStudio:  source("run_app.R")
# ================================================================

args      <- commandArgs(trailingOnly = TRUE)
.get_arg  <- function(flag, default) {
  idx <- which(args == flag)
  if (length(idx) > 0 && length(args) >= idx + 1) args[idx + 1] else default
}
APP_PORT   <- as.integer(.get_arg("--port", "3838"))
APP_HOST   <- .get_arg("--host", "127.0.0.1")
APP_LAUNCH <- !("--no-browser" %in% args)

cat("=============================================================\n")
cat("  API Data Retrieval & Analysis  –  R Shiny App  v2.0.0\n")
cat("=============================================================\n")

if (!file.exists("global.R") || !file.exists("server.R") || !file.exists("ui.R"))
  stop("run_app.R must be run from inside the openaq_app/ directory.")

if (is.null(getOption("repos")) ||
    identical(getOption("repos"), c(CRAN = "@CRAN@")))
  options(repos = c(CRAN = "https://cloud.r-project.org"))

required_pkgs <- c(
  "shiny","shinydashboard","shinyjs","shinyWidgets",
  "leaflet","leaflet.extras","DT","openair",
  "dplyr","tidyr","lubridate","httr","jsonlite",
  "RColorBrewer","ggplot2","scales","gridExtra",
  "remotes","zip","openxlsx"
)

cat("\nChecking dependencies ...\n")
missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  cat("  Installing:", paste(missing_pkgs, collapse=", "), "\n")
  tryCatch(install.packages(missing_pkgs, quiet=TRUE),
           error = function(e) warning("Could not install: ",
                                       paste(missing_pkgs, collapse=", ")))
}
cat("  All dependencies OK.\n\n")

cat("  R version :", paste0(R.version$major,".",R.version$minor), "\n")
tryCatch(cat("  shiny     :", as.character(packageVersion("shiny")),   "\n"), error=function(e) NULL)
tryCatch(cat("  openair   :", as.character(packageVersion("openair")), "\n"), error=function(e) NULL)
tryCatch(cat("  openxlsx  :", as.character(packageVersion("openxlsx")),"\n"), error=function(e) NULL)
cat("\n")

cat(sprintf("  Starting app on  http://%s:%d\n", APP_HOST, APP_PORT))
cat("  Press Ctrl+C to stop.\n")
cat("=============================================================\n\n")

shiny::runApp(".", host=APP_HOST, port=APP_PORT,
              launch.browser=APP_LAUNCH, display.mode="normal")
