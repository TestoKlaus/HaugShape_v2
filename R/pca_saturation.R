#' Compute PCA Saturation Curve
#'
#' Analyzes whether the morphospace is saturated by calculating variance metrics
#' across different sample sizes using bootstrap resampling. This helps determine
#' if adding new specimens will likely expand the morphospace.
#'
#' @param pca_object A PCA object (from PCA() function) or a matrix of PC scores
#' @param sample_sizes Numeric vector of sample sizes to test. Can be absolute numbers
#'   or proportions (0-1). Default: seq(0.1, 1, 0.1) for 10%, 20%, ..., 100%
#' @param bootstrap_iterations Number of bootstrap resampling iterations per sample size.
#'   Default: 200
#' @param pcs_to_analyze Number of principal components to include in analysis.
#'   Default: NULL (uses all available PCs with non-zero variance)
#' @param metric Variance metric to compute. Options: "total_variance", "pc1_variance",
#'   "cumulative_variance". Default: c("total_variance", "cumulative_variance")
#' @param min_sample_size Minimum sample size to test. Default: 5
#' @param seed Random seed for reproducibility. Default: NULL
#' @param parallel Use parallel processing for bootstrap. Default: FALSE
#' @param n_cores Number of cores for parallel processing. Default: NULL (uses all available - 1)
#'
#' @return A list with two elements:
#'   \item{saturation_data}{Data frame with columns: sample_size, metric_type, mean, median, sd, q025, q975}
#'   \item{parameters}{List of analysis parameters used}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load PCA results
#' pca_result <- readRDS("pca_analysis.rds")
#'
#' # Compute saturation curve
#' saturation <- compute_pca_saturation(
#'   pca_result,
#'   sample_sizes = seq(0.1, 1, 0.1),
#'   bootstrap_iterations = 200
#' )
#'
#' # Plot results
#' plot_pca_saturation(saturation)
#' }
compute_pca_saturation <- function(pca_object,
                                   sample_sizes = seq(0.1, 1, 0.1),
                                   bootstrap_iterations = 200,
                                   pcs_to_analyze = NULL,
                                   metric = c("total_variance", "cumulative_variance"),
                                   min_sample_size = 5,
                                   seed = NULL,
                                   parallel = FALSE,
                                   n_cores = NULL) {
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Extract PC scores from PCA object or use matrix directly
  if (inherits(pca_object, "PCA") || inherits(pca_object, "prcomp")) {
    pc_scores <- pca_object$x
  } else if (is.matrix(pca_object) || is.data.frame(pca_object)) {
    pc_scores <- as.matrix(pca_object)
  } else {
    stop("pca_object must be a PCA object, prcomp object, or a matrix of PC scores")
  }
  
  # Get number of specimens and PCs
  n_specimens <- nrow(pc_scores)
  n_pcs <- ncol(pc_scores)
  
  # Determine PCs to analyze
  if (is.null(pcs_to_analyze)) {
    pcs_to_analyze <- n_pcs
  } else {
    pcs_to_analyze <- min(pcs_to_analyze, n_pcs)
  }
  
  # Subset PC scores to selected PCs
  pc_scores <- pc_scores[, 1:pcs_to_analyze, drop = FALSE]
  
  # Convert proportions to absolute sample sizes
  sample_sizes_abs <- ifelse(sample_sizes <= 1, 
                              round(sample_sizes * n_specimens), 
                              sample_sizes)
  
  # Filter out sample sizes below minimum
  sample_sizes_abs <- sample_sizes_abs[sample_sizes_abs >= min_sample_size]
  sample_sizes_abs <- unique(sort(sample_sizes_abs))
  
  # Ensure maximum sample size doesn't exceed total specimens
  sample_sizes_abs <- sample_sizes_abs[sample_sizes_abs <= n_specimens]
  
  if (length(sample_sizes_abs) == 0) {
    stop(paste0("No valid sample sizes found. Minimum sample size is ", 
                min_sample_size, " and total specimens is ", n_specimens))
  }
  
  # Match metric argument
  metric <- match.arg(metric, several.ok = TRUE)
  
  # Helper function to compute variance metrics for a subsample
  compute_variance_metrics <- function(subsample_scores, metrics_to_compute) {
    results <- list()
    
    # Compute variance per PC
    pc_variances <- apply(subsample_scores, 2, var)
    total_var <- sum(pc_variances)
    
    if ("total_variance" %in% metrics_to_compute) {
      results$total_variance <- total_var
    }
    
    if ("pc1_variance" %in% metrics_to_compute) {
      results$pc1_variance <- pc_variances[1]
    }
    
    if ("cumulative_variance" %in% metrics_to_compute) {
      # Cumulative variance of all selected PCs
      results$cumulative_variance <- total_var
    }
    
    return(results)
  }
  
  # Function to run bootstrap for a single sample size
  bootstrap_sample_size <- function(sample_size, scores, n_iter, metrics) {
    n_samples <- nrow(scores)
    
    # Store results for each bootstrap iteration
    iter_results <- vector("list", n_iter)
    
    for (i in 1:n_iter) {
      # Resample with replacement
      boot_indices <- sample(1:n_samples, size = sample_size, replace = TRUE)
      boot_scores <- scores[boot_indices, , drop = FALSE]
      
      # Compute metrics
      iter_results[[i]] <- compute_variance_metrics(boot_scores, metrics)
    }
    
    # Aggregate results across iterations for each metric
    aggregated <- list()
    for (m in metrics) {
      metric_values <- sapply(iter_results, function(x) x[[m]])
      
      aggregated[[m]] <- data.frame(
        sample_size = sample_size,
        metric_type = m,
        mean = mean(metric_values, na.rm = TRUE),
        median = median(metric_values, na.rm = TRUE),
        sd = sd(metric_values, na.rm = TRUE),
        q025 = quantile(metric_values, 0.025, na.rm = TRUE),
        q975 = quantile(metric_values, 0.975, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
    
    return(do.call(rbind, aggregated))
  }
  
  # Run bootstrap analysis across all sample sizes
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel processing
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export necessary objects to cluster
    parallel::clusterExport(cl, 
                           c("pc_scores", "bootstrap_iterations", "metric",
                             "compute_variance_metrics", "bootstrap_sample_size"),
                           envir = environment())
    
    results_list <- parallel::parLapply(cl, sample_sizes_abs, function(ss) {
      bootstrap_sample_size(ss, pc_scores, bootstrap_iterations, metric)
    })
    
  } else {
    # Sequential processing
    results_list <- lapply(sample_sizes_abs, function(ss) {
      bootstrap_sample_size(ss, pc_scores, bootstrap_iterations, metric)
    })
  }
  
  # Combine results into single data frame
  saturation_data <- do.call(rbind, results_list)
  rownames(saturation_data) <- NULL
  
  # Calculate proportion of total specimens
  saturation_data$sample_proportion <- saturation_data$sample_size / n_specimens
  
  # Prepare parameters list
  parameters <- list(
    n_specimens = n_specimens,
    n_pcs_analyzed = pcs_to_analyze,
    sample_sizes = sample_sizes_abs,
    bootstrap_iterations = bootstrap_iterations,
    metrics = metric,
    min_sample_size = min_sample_size,
    parallel = parallel
  )
  
  # Return results
  result <- list(
    saturation_data = saturation_data,
    parameters = parameters
  )
  
  class(result) <- c("pca_saturation", "list")
  
  return(result)
}


#' Plot PCA Saturation Curve
#'
#' Creates a saturation curve plot showing how morphospace variance changes
#' with increasing sample size.
#'
#' @param saturation_result Output from compute_pca_saturation()
#' @param x_axis Type of x-axis: "absolute" (number of specimens) or "proportion" (0-1)
#' @param show_ci Show confidence intervals (95% quantiles). Default: TRUE
#' @param show_points Show individual data points. Default: TRUE
#' @param colors Named vector of colors for different metrics. Default: NULL (uses default colors)
#' @param theme_name Theme to apply: "Haug", "inverted_Haug", "publication", or "default"
#' @param title Plot title. Default: "PCA Saturation Curve"
#' @param subtitle Plot subtitle. Default: NULL
#'
#' @return A ggplot2 object
#'
#' @export
plot_pca_saturation <- function(saturation_result,
                                x_axis = c("absolute", "proportion"),
                                show_ci = TRUE,
                                show_points = TRUE,
                                colors = NULL,
                                theme_name = "Haug",
                                title = "PCA Saturation Curve",
                                subtitle = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }
  
  x_axis <- match.arg(x_axis)
  
  # Extract data
  data <- saturation_result$saturation_data
  params <- saturation_result$parameters
  
  # Determine x variable
  x_var <- if (x_axis == "proportion") "sample_proportion" else "sample_size"
  x_lab <- if (x_axis == "proportion") "Sample Size (Proportion of Total)" else "Number of Specimens"
  
  # Default colors if not provided
  if (is.null(colors)) {
    colors <- c(
      total_variance = "#2E86AB",
      cumulative_variance = "#A23B72",
      pc1_variance = "#F18F01"
    )
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = "mean", 
                                                  color = "metric_type", 
                                                  fill = "metric_type"))
  
  # Add confidence interval ribbon if requested
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "q025", ymax = "q975"),
                                   alpha = 0.2, color = NA)
  }
  
  # Add line
  p <- p + ggplot2::geom_line(size = 1.2)
  
  # Add points if requested
  if (show_points) {
    p <- p + ggplot2::geom_point(size = 2.5)
  }
  
  # Add labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    x = x_lab,
    y = "Variance",
    color = "Metric",
    fill = "Metric"
  )
  
  # Apply theme
  if (theme_name == "Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 11, face = "bold"),
        legend.text = ggplot2::element_text(size = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  } else if (theme_name == "inverted_Haug") {
    p <- p + ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold", color = "white"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "white"),
        axis.title = ggplot2::element_text(size = 12, face = "bold", color = "white"),
        axis.text = ggplot2::element_text(size = 10, color = "white"),
        legend.title = ggplot2::element_text(size = 11, face = "bold", color = "white"),
        legend.text = ggplot2::element_text(size = 10, color = "white"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "gray30"),
        panel.background = ggplot2::element_rect(fill = "black", color = NA),
        plot.background = ggplot2::element_rect(fill = "black", color = NA)
      )
  } else if (theme_name == "publication") {
    p <- p + ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 9),
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 9)
      )
  }
  
  # Apply custom colors if provided
  if (!is.null(colors)) {
    p <- p + ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  }
  
  return(p)
}


#' Print method for pca_saturation objects
#'
#' @param x A pca_saturation object
#' @param ... Additional arguments (not used)
#'
#' @export
print.pca_saturation <- function(x, ...) {
  cat("PCA Saturation Analysis\n")
  cat("=======================\n\n")
  cat("Parameters:\n")
  cat(sprintf("  Total specimens: %d\n", x$parameters$n_specimens))
  cat(sprintf("  PCs analyzed: %d\n", x$parameters$n_pcs_analyzed))
  cat(sprintf("  Bootstrap iterations: %d\n", x$parameters$bootstrap_iterations))
  cat(sprintf("  Sample sizes tested: %d to %d\n", 
              min(x$parameters$sample_sizes), 
              max(x$parameters$sample_sizes)))
  cat(sprintf("  Metrics: %s\n", paste(x$parameters$metrics, collapse = ", ")))
  cat("\nResults:\n")
  print(head(x$saturation_data, 10))
  if (nrow(x$saturation_data) > 10) {
    cat(sprintf("\n... and %d more rows\n", nrow(x$saturation_data) - 10))
  }
  invisible(x)
}
