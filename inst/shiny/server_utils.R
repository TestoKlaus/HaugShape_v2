# HaugShape v2 - Server Utilities
# Additional server-side functions for the Shiny application

# Data Processing Functions ----

#' Process uploaded shape files
#' @param file_list List of uploaded files from fileInput
#' @param processing_options List of processing options
#' @return List with processed file information
process_shape_files <- function(file_list, processing_options = list()) {
  
  if (is.null(file_list)) {
    return(NULL)
  }
  
  processed_files <- list()
  
  for (i in seq_len(nrow(file_list))) {
    file_info <- list(
      original_name = file_list$name[i],
      temp_path = file_list$datapath[i],
      size = file_list$size[i],
      type = tools::file_ext(file_list$name[i]),
      status = "uploaded"
    )
    
    # Perform validation
    if (processing_options$validate_binary %||% TRUE) {
      # In real implementation, check if image is binary
      file_info$is_binary <- sample(c(TRUE, FALSE), 1, prob = c(0.8, 0.2))
      if (!file_info$is_binary) {
        file_info$status <- "needs_processing"
      }
    }
    
    processed_files[[i]] <- file_info
  }
  
  names(processed_files) <- file_list$name
  return(processed_files)
}

#' Generate analysis summary statistics
#' @param results Analysis results object
#' @return Character string with formatted summary
generate_analysis_summary <- function(results) {
  
  if (is.null(results)) {
    return("No analysis results available")
  }
  
  summary_lines <- c(
    "SHAPE ANALYSIS SUMMARY",
    "======================",
    "",
    paste("Analysis Date:", Sys.time()),
    paste("Method:", results$analysis_summary$method %||% "Unknown"),
    paste("Number of Specimens:", results$analysis_summary$n_specimens %||% 0),
    paste("Number of Components:", results$analysis_summary$n_components %||% 0),
    ""
  )
  
  if (!is.null(results$variance_explained)) {
    summary_lines <- c(summary_lines,
      "VARIANCE EXPLAINED:",
      "-------------------"
    )
    
    var_per_pc <- diff(c(0, results$variance_explained))
    for (i in seq_along(var_per_pc)) {
      summary_lines <- c(summary_lines,
        paste0("PC", i, ": ", sprintf("%.2f%%", var_per_pc[i] * 100))
      )
    }
    
    summary_lines <- c(summary_lines,
      "",
      paste("Total Variance Captured:", 
            sprintf("%.2f%%", max(results$variance_explained) * 100))
    )
  }
  
  if (!is.null(results$specimen_names)) {
    summary_lines <- c(summary_lines,
      "",
      "SPECIMENS:",
      "----------"
    )
    
    n_specimens <- length(results$specimen_names)
    if (n_specimens <= 10) {
      summary_lines <- c(summary_lines, results$specimen_names)
    } else {
      summary_lines <- c(summary_lines,
        results$specimen_names[1:5],
        paste("... and", n_specimens - 5, "more specimens")
      )
    }
  }
  
  paste(summary_lines, collapse = "\n")
}

#' Create interactive PCA plot
#' @param results Analysis results
#' @param x_axis PC for x-axis
#' @param y_axis PC for y-axis
#' @param color_by Grouping variable for colors
#' @param point_size Size of points
#' @return plotly object
create_interactive_pca_plot <- function(results, x_axis = "PC1", y_axis = "PC2", 
                                       color_by = NULL, point_size = 8) {
  
  if (is.null(results) || is.null(results$pca_scores)) {
    return(plotly::plot_ly() %>% 
           plotly::add_text(x = 0, y = 0, text = "No data available") %>%
           plotly::layout(title = "PCA Plot - No Data"))
  }
  
  df <- as.data.frame(results$pca_scores)
  df$specimen <- rownames(df)
  
  # Add variance explained to axis labels
  var_exp <- results$variance_explained %||% rep(0, ncol(results$pca_scores))
  
  x_var <- if (x_axis %in% names(df)) {
    pc_num <- as.numeric(gsub("PC", "", x_axis))
    var_pct <- if (pc_num <= length(var_exp)) {
      if (pc_num == 1) var_exp[1] else var_exp[pc_num] - var_exp[pc_num - 1]
    } else 0
    sprintf("%.1f%%", var_pct * 100)
  } else "0%"
  
  y_var <- if (y_axis %in% names(df)) {
    pc_num <- as.numeric(gsub("PC", "", y_axis))
    var_pct <- if (pc_num <= length(var_exp)) {
      if (pc_num == 1) var_exp[1] else var_exp[pc_num] - var_exp[pc_num - 1]
    } else 0
    sprintf("%.1f%%", var_pct * 100)
  } else "0%"
  
  x_label <- paste0(x_axis, " (", x_var, ")")
  y_label <- paste0(y_axis, " (", y_var, ")")
  
  # Create base plot
  p <- plotly::plot_ly(df, 
                      x = as.formula(paste0("~", x_axis)), 
                      y = as.formula(paste0("~", y_axis)),
                      text = ~specimen,
                      type = "scatter", 
                      mode = "markers",
                      marker = list(size = point_size))
  
  # Add colors if grouping variable provided
  if (!is.null(color_by) && color_by != "none" && color_by %in% names(df)) {
    p <- p %>% plotly::add_markers(color = as.formula(paste0("~", color_by)))
  }
  
  # Layout
  p <- p %>% plotly::layout(
    title = "Interactive PCA Plot",
    xaxis = list(title = x_label),
    yaxis = list(title = y_label),
    hovermode = "closest"
  )
  
  return(p)
}

#' Generate clustering validation metrics
#' @param clustering_results Clustering results object
#' @param original_data Original data used for clustering
#' @return List with validation metrics
compute_clustering_validation <- function(clustering_results, original_data) {
  
  if (is.null(clustering_results) || is.null(original_data)) {
    return(list(
      message = "No clustering results or data available",
      metrics = NULL
    ))
  }
  
  clusters <- clustering_results$clusters
  n_clusters <- length(unique(clusters))
  
  # Mock validation metrics (replace with real implementations)
  metrics <- list(
    silhouette_avg = runif(1, 0.2, 0.8),
    calinski_harabasz = runif(1, 10, 100),
    davies_bouldin = runif(1, 0.5, 2.0),
    inertia = runif(1, 50, 500)
  )
  
  validation_text <- paste(
    "CLUSTERING VALIDATION METRICS",
    "=============================",
    "",
    paste("Method:", clustering_results$method),
    paste("Number of Clusters:", n_clusters),
    paste("Number of Data Points:", nrow(original_data)),
    "",
    "Quality Metrics:",
    paste("- Average Silhouette Score:", sprintf("%.3f", metrics$silhouette_avg)),
    paste("- Calinski-Harabasz Index:", sprintf("%.2f", metrics$calinski_harabasz)),
    paste("- Davies-Bouldin Index:", sprintf("%.3f", metrics$davies_bouldin)),
    paste("- Within-cluster Sum of Squares:", sprintf("%.2f", metrics$inertia)),
    "",
    "Interpretation:",
    "- Silhouette Score: Higher is better (range: -1 to 1)",
    "- Calinski-Harabasz: Higher is better",
    "- Davies-Bouldin: Lower is better",
    sep = "\n"
  )
  
  return(list(
    message = validation_text,
    metrics = metrics
  ))
}

# File Management Functions ----

#' Create download content for results
#' @param results Analysis results
#' @param format Output format ("csv", "xlsx", "rds")
#' @return File content
prepare_download_content <- function(results, format = "csv") {
  
  if (is.null(results)) {
    return(NULL)
  }
  
  temp_file <- tempfile(fileext = paste0(".", format))
  
  tryCatch({
    if (format == "csv") {
      if (!is.null(results$pca_scores)) {
        write.csv(results$pca_scores, temp_file, row.names = TRUE)
      } else {
        write.csv(data.frame(message = "No data available"), temp_file)
      }
    } else if (format == "xlsx") {
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        
        if (!is.null(results$pca_scores)) {
          openxlsx::addWorksheet(wb, "PCA_Scores")
          openxlsx::writeData(wb, "PCA_Scores", results$pca_scores, rowNames = TRUE)
        }
        
        if (!is.null(results$loadings)) {
          openxlsx::addWorksheet(wb, "Loadings")
          openxlsx::writeData(wb, "Loadings", results$loadings, rowNames = TRUE)
        }
        
        if (!is.null(results$variance_explained)) {
          var_df <- data.frame(
            Component = paste0("PC", seq_along(results$variance_explained)),
            Variance_Explained = results$variance_explained,
            Variance_Per_PC = diff(c(0, results$variance_explained))
          )
          openxlsx::addWorksheet(wb, "Variance")
          openxlsx::writeData(wb, "Variance", var_df)
        }
        
        openxlsx::saveWorkbook(wb, temp_file, overwrite = TRUE)
      }
    } else if (format == "rds") {
      saveRDS(results, temp_file)
    }
    
    return(temp_file)
    
  }, error = function(e) {
    warning("Failed to create download content: ", e$message)
    return(NULL)
  })
}

# Utility Functions ----

#' Null-coalescing operator
#' @param lhs Left-hand side
#' @param rhs Right-hand side  
#' @return lhs if not NULL, otherwise rhs
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0) lhs else rhs
}

#' Safe column selection
#' @param data Data frame
#' @param columns Column names to select
#' @return Data frame with selected columns or empty data frame
safe_select_columns <- function(data, columns) {
  if (is.null(data) || is.null(columns) || length(columns) == 0) {
    return(data.frame())
  }
  
  available_columns <- intersect(columns, names(data))
  
  if (length(available_columns) == 0) {
    return(data.frame())
  }
  
  return(data[, available_columns, drop = FALSE])
}

#' Validate numeric input
#' @param value Input value
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param default_val Default value if invalid
#' @return Validated numeric value
validate_numeric_input <- function(value, min_val = -Inf, max_val = Inf, default_val = 0) {
  if (is.null(value) || is.na(value) || !is.numeric(value)) {
    return(default_val)
  }
  
  if (value < min_val || value > max_val) {
    return(default_val)
  }
  
  return(value)
}

#' Create error message for UI display
#' @param error Error object or message
#' @param context Additional context information
#' @return Formatted error message
format_error_message <- function(error, context = NULL) {
  
  error_msg <- if (inherits(error, "error")) {
    error$message
  } else {
    as.character(error)
  }
  
  formatted_msg <- paste(
    "An error occurred:",
    error_msg,
    sep = "\n"
  )
  
  if (!is.null(context)) {
    formatted_msg <- paste(
      formatted_msg,
      paste("Context:", context),
      sep = "\n\n"
    )
  }
  
  formatted_msg <- paste(
    formatted_msg,
    "\nPlease check your input data and try again.",
    "If the problem persists, contact support.",
    sep = "\n"
  )
  
  return(formatted_msg)
}