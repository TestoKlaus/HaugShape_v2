# HaugShape v2 Shiny App Global Settings
# Global variables, functions, and settings used across the Shiny application

# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(shinyWidgets)
  library(shinycssloaders)
  library(colourpicker)
  library(ggplot2)
  library(dplyr)
})

# Global Constants ----

# Supported file formats
SUPPORTED_IMAGE_FORMATS <- c(".png", ".jpg", ".jpeg", ".tiff", ".tif", ".bmp")
SUPPORTED_DATA_FORMATS <- c(".csv", ".xlsx", ".xls", ".txt")

# Default color palettes
COLOR_PALETTES <- list(
  "default" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"),
  "viridis" = viridis::viridis(5),
  "RdYlBu" = RColorBrewer::brewer.pal(5, "RdYlBu"),
  "Set1" = RColorBrewer::brewer.pal(5, "Set1"),
  "Dark2" = RColorBrewer::brewer.pal(5, "Dark2")
)

# Analysis methods
ANALYSIS_METHODS <- list(
  "efa" = "Elliptical Fourier Analysis",
  "pca" = "Principal Component Analysis", 
  "both" = "Both EFA and PCA"
)

# Clustering algorithms
CLUSTERING_METHODS <- list(
  "kmeans" = "K-means",
  "hierarchical" = "Hierarchical",
  "gaussian_mixture" = "Gaussian Mixture",
  "dbscan" = "DBSCAN",
  "pam" = "PAM (Partitioning Around Medoids)"
)

# Hull algorithms
HULL_ALGORITHMS <- list(
  "convex" = "Convex Hull",
  "alpha" = "Alpha Hull",
  "concave" = "Concave Hull"
)

# Global Helper Functions ----

#' Validate file upload
#' @param file_input File input from Shiny
#' @param allowed_formats Character vector of allowed file extensions
#' @return List with validation results
validate_file_upload <- function(file_input, allowed_formats) {
  if (is.null(file_input)) {
    return(list(valid = FALSE, message = "No file uploaded"))
  }
  
  extensions <- tolower(tools::file_ext(file_input$name))
  invalid_files <- file_input$name[!paste0(".", extensions) %in% allowed_formats]
  
  if (length(invalid_files) > 0) {
    return(list(
      valid = FALSE, 
      message = paste("Invalid file formats:", paste(invalid_files, collapse = ", "))
    ))
  }
  
  return(list(valid = TRUE, message = "All files valid"))
}

#' Generate safe filename
#' @param name Character string
#' @return Safe filename string
safe_filename <- function(name) {
  # Remove special characters and spaces
  name <- gsub("[^A-Za-z0-9_.-]", "_", name)
  # Remove multiple underscores
  name <- gsub("_{2,}", "_", name)
  # Remove leading/trailing underscores
  name <- gsub("^_+|_+$", "", name)
  return(name)
}

#' Format file size for display
#' @param size_bytes Numeric file size in bytes
#' @return Character string with formatted size
format_file_size <- function(size_bytes) {
  if (size_bytes < 1024) {
    return(paste(size_bytes, "B"))
  } else if (size_bytes < 1024^2) {
    return(paste(round(size_bytes / 1024, 1), "KB"))
  } else if (size_bytes < 1024^3) {
    return(paste(round(size_bytes / 1024^2, 1), "MB"))
  } else {
    return(paste(round(size_bytes / 1024^3, 1), "GB"))
  }
}

#' Create progress notification
#' @param message Character string for the message
#' @param progress Numeric progress value (0-1)
#' @return Notification ID
show_progress_notification <- function(message, progress = NULL) {
  if (!is.null(progress)) {
    message <- paste0(message, " (", round(progress * 100), "%)")
  }
  
  showNotification(
    message,
    type = "message",
    duration = NULL
  )
}

#' Generate mock data for demonstration
#' @param n_samples Number of samples
#' @param n_features Number of features
#' @return List with mock analysis results
generate_mock_results <- function(n_samples = 50, n_features = 10) {
  # Generate mock PCA results
  pca_scores <- matrix(rnorm(n_samples * n_features), ncol = n_features)
  colnames(pca_scores) <- paste0("PC", 1:n_features)
  rownames(pca_scores) <- paste0("Sample_", 1:n_samples)
  
  # Generate mock loadings
  loadings <- matrix(rnorm(20 * n_features), ncol = n_features)
  colnames(loadings) <- paste0("PC", 1:n_features)
  rownames(loadings) <- paste0("Variable_", 1:20)
  
  # Generate variance explained
  var_exp <- sort(runif(n_features, 0.05, 0.3), decreasing = TRUE)
  var_exp <- cumsum(var_exp / sum(var_exp))
  
  return(list(
    pca_scores = pca_scores,
    loadings = loadings,
    variance_explained = var_exp,
    specimen_names = rownames(pca_scores),
    analysis_summary = list(
      method = "mock",
      n_specimens = n_samples,
      n_components = n_features,
      total_variance = max(var_exp)
    )
  ))
}

#' Create standardized plot theme
#' @param theme_name Character string specifying theme
#' @return ggplot2 theme object
get_plot_theme <- function(theme_name = "classic") {
  switch(theme_name,
    "classic" = theme_classic(),
    "minimal" = theme_minimal(),
    "publication" = theme_bw() + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      legend.key = element_blank()
    ),
    "dark" = theme_dark(),
    theme_minimal()  # default
  )
}

#' Validate data for analysis
#' @param data Data frame or matrix
#' @param min_samples Minimum number of samples required
#' @param min_features Minimum number of features required
#' @return List with validation results
validate_analysis_data <- function(data, min_samples = 5, min_features = 2) {
  if (is.null(data)) {
    return(list(valid = FALSE, message = "No data provided"))
  }
  
  if (!is.data.frame(data) && !is.matrix(data)) {
    return(list(valid = FALSE, message = "Data must be a data frame or matrix"))
  }
  
  if (nrow(data) < min_samples) {
    return(list(
      valid = FALSE, 
      message = paste("Minimum", min_samples, "samples required, got", nrow(data))
    ))
  }
  
  if (ncol(data) < min_features) {
    return(list(
      valid = FALSE, 
      message = paste("Minimum", min_features, "features required, got", ncol(data))
    ))
  }
  
  # Check for missing values
  if (any(is.na(data))) {
    n_missing <- sum(is.na(data))
    return(list(
      valid = FALSE, 
      message = paste("Data contains", n_missing, "missing values")
    ))
  }
  
  return(list(valid = TRUE, message = "Data validation passed"))
}

# Global CSS Styles ----
APP_CSS <- "
  .main-header .logo { 
    font-weight: bold; 
    font-size: 18px;
  }
  
  .box-header { 
    background: linear-gradient(45deg, #3c8dbc, #367fa9); 
  }
  
  .box-title { 
    color: white !important; 
    font-weight: bold; 
  }
  
  .progress-bar { 
    background-color: #3c8dbc; 
  }
  
  .btn-success { 
    background-color: #00a65a;
    border-color: #008d4c; 
  }
  
  .btn-warning { 
    background-color: #f39c12;
    border-color: #e08e0b; 
  }
  
  .sidebar-menu > li.active > a { 
    background-color: #367fa9; 
  }
  
  .nav-tabs-custom > .nav-tabs > li.active > a { 
    background-color: #3c8dbc; 
    color: white; 
  }
  
  .content-wrapper {
    min-height: calc(100vh - 50px);
  }
  
  .loading-overlay {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(255, 255, 255, 0.8);
    z-index: 9999;
    display: flex;
    justify-content: center;
    align-items: center;
  }
  
  .status-indicator {
    display: inline-block;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    margin-right: 8px;
  }
  
  .status-success { background-color: #28a745; }
  .status-warning { background-color: #ffc107; }
  .status-danger { background-color: #dc3545; }
  .status-info { background-color: #17a2b8; }
"

# Application metadata
APP_INFO <- list(
  name = "HaugShape v2",
  version = "2.0.0",
  description = "Comprehensive Morphometric Analysis Suite",
  authors = "HaugShape Development Team",
  license = "MIT",
  repository = "https://github.com/TestoKlaus/HaugShape_v2"
)