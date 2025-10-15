# Run HaugShape v2 Shiny Application
# 
# This script launches the HaugShape v2 Shiny application for interactive
# geometric morphometric analysis.

#' Launch HaugShape v2 Shiny Application
#'
#' Starts the interactive web interface for HaugShape v2, providing access to
#' all morphometric analysis tools through a user-friendly dashboard.
#'
#' @param host Character string specifying the host IP address. Default: "127.0.0.1"
#' @param port Integer specifying the port number. Default: NULL (auto-assign)
#' @param launch.browser Logical indicating whether to launch browser automatically. Default: TRUE
#' @param display.mode Character string specifying display mode. Options: "normal", "showcase". Default: "normal"
#'
#' @examples
#' \dontrun{
#' # Launch the app with default settings
#' run_haug_app()
#' 
#' # Launch on specific port
#' run_haug_app(port = 3838)
#' 
#' # Launch without auto-opening browser
#' run_haug_app(launch.browser = FALSE)
#' }
#'
#' @export
run_haug_app <- function(host = "127.0.0.1", 
                        port = NULL, 
                        launch.browser = TRUE, 
                        display.mode = "normal") {
  
  # Check required packages
  required_packages <- c(
    "shiny", "shinydashboard", "DT", "plotly", "shinyWidgets",
    "shinyFiles", "shinycssloaders", "colourpicker", "ggplot2",
    "dplyr", "readxl"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("The following required packages are missing: ", 
         paste(missing_packages, collapse = ", "), 
         "\nPlease install them using: install.packages(c(", 
         paste(paste0("'", missing_packages, "'"), collapse = ", "), "))",
         call. = FALSE)
  }
  
  # Get the app directory
  app_dir <- system.file("shiny", package = "HaugShape")

  if (app_dir == "") {
    # If package not installed, try local development version (inst/shiny)
    inst_app_dir <- file.path(getwd(), "inst", "shiny")
    if (dir.exists(inst_app_dir)) {
      app_dir <- inst_app_dir
    } else if (file.exists(file.path(getwd(), "app.R"))) {
      # Fallback: run from project root that contains app.R
      app_dir <- getwd()
    } else {
      stop(
        "Cannot find Shiny app directory. Please ensure HaugShape package is properly installed or that app.R exists in the project root.",
        call. = FALSE
      )
    }
  }
  
  # Launch the app
  message("Starting HaugShape v2 Shiny Application...")
  message("App directory: ", app_dir)
  
  if (!is.null(port)) {
    message("Launching on http://", host, ":", port)
  }
  
  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    display.mode = display.mode
  )
}

# Do not auto-run when this file is sourced by Shiny's support loader.
# Call run_haug_app() explicitly from user code when needed.