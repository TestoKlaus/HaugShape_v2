# Suppress R CMD check notes for ggplot2 NSE
utils::globalVariables(c("x", "y", "certainty", "group"))

#' Detect Morphospace Gaps with Uncertainty Quantification
#'
#' @param pca_scores Data frame or matrix with PC scores
#' @param uncertainty Proportion of axis range for uncertainty radius
#' @param grid_resolution Number of grid cells along each axis
#' @param monte_carlo_iterations Number of Monte Carlo replicates
#' @param bootstrap_iterations Number of bootstrap resamples
#' @param certainty_thresholds Gap certainty thresholds for polygon extraction
#' @param pc_pairs Optional matrix specifying PC pairs to analyze
#' @param max_pcs Maximum PC to include in automatic pair generation
#' @param hull_type Type of hull for domain constraint
#' @param alpha_value Alpha parameter for alphahull
#' @param hull_buffer Proportional buffer to add around hull
#' @param use_parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param uncertainty_type Type of uncertainty model
#' @param occupancy_method Method to determine cell occupancy
#' @param occupancy_radius Radius for occupancy detection
#' @param progress_callback Optional function for progress updates
#' @param verbose Print progress messages
#'
#' @return List of class morphospace_gaps with results
#' @export
#'
#' @details
#' The function implements a rigorous two-stage uncertainty analysis:
#'
#' \strong{Stage 1: Measurement Uncertainty (Monte Carlo)}
#' Each observed point is treated as uncertain within ±u (uncertainty * axis range).
#' For each Monte Carlo iteration, points are perturbed according to the
#' uncertainty model (Gaussian or uniform). Grid cells are marked as occupied
#' if any perturbed point falls within the occupancy radius. The gap probability
#' for each cell is the proportion of iterations where it remains unoccupied.
#'
#' \strong{Stage 2: Sampling Uncertainty (Bootstrap)}
#' To assess whether gaps are artifacts of limited sampling, the dataset is
#' resampled (with replacement) B times. For each resample, the Monte Carlo
#' procedure is repeated and a binary gap mask is created. The gap stability
#' for each cell is the proportion of bootstrap iterations where it was
#' classified as a gap.
#'
#' \strong{Combined Certainty}
#' gap_certainty = gap_probability * gap_stability
#' This represents the probability that a region is genuinely unoccupied,
#' accounting for both measurement and sampling uncertainty.
#'
#' @examples
#' \dontrun{
#' # Assuming pca_result from PCA() contains $x with PC scores
#' gaps <- detect_morphospace_gaps(
#'   pca_scores = pca_result$x,
#'   uncertainty = 0.05,
#'   grid_resolution = 150,
#'   monte_carlo_iterations = 100,
#'   bootstrap_iterations = 200,
#'   max_pcs = 3
#' )
#'
#' # View summary
#' print(gaps$summary_table)
#'
#' # Access results for PC1-PC2
#' pc1_pc2_gaps <- gaps$results$`PC1-PC2`
#' plot(pc1_pc2_gaps$gap_certainty)
#' }
#'
#' @export
detect_morphospace_gaps <- function(pca_scores,
                                    uncertainty = 0.05,
                                    grid_resolution = 150,
                                    monte_carlo_iterations = 100,
                                    bootstrap_iterations = 200,
                                    certainty_thresholds = c(0.80, 0.90, 0.95),
                                    pc_pairs = NULL,
                                    max_pcs = 4,
                                    hull_type = "alpha",
                                    alpha_value = NULL,
                                    hull_buffer = 0.05,
                                    use_parallel = FALSE,
                                    n_cores = NULL,
                                    uncertainty_type = "gaussian",
                                    occupancy_method = "radius",
                                    occupancy_radius = 1.5,
                                    progress_callback = NULL,
                                    verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(pca_scores) && !is.matrix(pca_scores)) {
    stop("pca_scores must be a data frame or matrix")
  }
  
  # Convert to data frame if matrix
  if (is.matrix(pca_scores)) {
    pca_scores <- as.data.frame(pca_scores)
  }
  
  # Check for PC columns
  pc_cols <- grep("^PC[0-9]+$", colnames(pca_scores), value = TRUE)
  if (length(pc_cols) == 0) {
    stop("No columns matching pattern 'PC1', 'PC2', etc. found in pca_scores")
  }
  
  # Extract numeric PC indices
  pc_indices <- as.integer(sub("PC", "", pc_cols))
  available_pcs <- sort(pc_indices)
  
  if (verbose) {
    cat(sprintf("Found %d PC columns: %s\n", 
                length(available_pcs), 
                paste(pc_cols, collapse = ", ")))
  }
  
  # Generate PC pairs if not provided
  if (is.null(pc_pairs)) {
    max_pcs <- min(max_pcs, max(available_pcs))
    valid_pcs <- available_pcs[available_pcs <= max_pcs]
    
    if (length(valid_pcs) < 2) {
      stop("Need at least 2 PCs for gap analysis")
    }
    
    # Generate all pairwise combinations
    pc_pairs <- t(combn(valid_pcs, 2))
    
    if (verbose) {
      cat(sprintf("Analyzing %d PC pairs (PC1 to PC%d)\n", 
                  nrow(pc_pairs), max_pcs))
    }
    
    if (!is.null(progress_callback)) {
      progress_callback(sprintf("Analyzing %d PC pairs...", nrow(pc_pairs)), 0)
    }
  } else {
    # Validate provided pairs
    if (!is.matrix(pc_pairs) || ncol(pc_pairs) != 2) {
      stop("pc_pairs must be a matrix with 2 columns")
    }
  }
  
  # Check required packages
  if (hull_type == "alpha") {
    if (!requireNamespace("alphahull", quietly = TRUE)) {
      warning("Package 'alphahull' not available. Falling back to convex hull.")
      hull_type <- "convex"
    }
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install with: install.packages('sf')")
  }
  
  # Setup parallel processing if requested
  if (use_parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available. Using sequential processing.")
      use_parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      if (verbose) {
        cat(sprintf("Using parallel processing with %d cores\n", n_cores))
      }
    }
  }
  
  # Store parameters
  parameters <- list(
    uncertainty = uncertainty,
    grid_resolution = grid_resolution,
    monte_carlo_iterations = monte_carlo_iterations,
    bootstrap_iterations = bootstrap_iterations,
    certainty_thresholds = certainty_thresholds,
    hull_type = hull_type,
    alpha_value = alpha_value,
    hull_buffer = hull_buffer,
    uncertainty_type = uncertainty_type,
    occupancy_method = occupancy_method,
    occupancy_radius = occupancy_radius,
    timestamp = Sys.time()
  )
  
  # Initialize results storage
  results <- list()
  summary_rows <- list()
  
  # Analyze each PC pair
  for (i in seq_len(nrow(pc_pairs))) {
    pc_x <- pc_pairs[i, 1]
    pc_y <- pc_pairs[i, 2]
    pair_name <- sprintf("PC%d-PC%d", pc_x, pc_y)
    
    # Construct column names
    col_x <- sprintf("PC%d", pc_x)
    col_y <- sprintf("PC%d", pc_y)
    
    if (verbose) {
      cat(sprintf("\n=== Analyzing %s (%d/%d) ===\n", 
                  pair_name, i, nrow(pc_pairs)))
    }
      
    if (!is.null(progress_callback)) {
      progress_callback(
        sprintf("PC%d-PC%d (%d/%d): Monte Carlo + Bootstrap", 
                pc_x, pc_y, i, nrow(pc_pairs)),
        0.8 / nrow(pc_pairs)
      )
    }
    
    if (!col_x %in% colnames(pca_scores) || !col_y %in% colnames(pca_scores)) {
      warning(sprintf("Columns %s or %s not found. Skipping %s", 
                     col_x, col_y, pair_name))
      next
    }
    
    points <- pca_scores[, c(col_x, col_y)]
    colnames(points) <- c("x", "y")
    points <- na.omit(points)
    
    if (nrow(points) < 3) {
      warning(sprintf("Insufficient points for %s. Skipping.", pair_name))
      next
    }
    
    # Analyze this PC pair
    pair_result <- .analyze_pc_pair(
      points = points,
      uncertainty = uncertainty,
      grid_resolution = grid_resolution,
      monte_carlo_iterations = monte_carlo_iterations,
      bootstrap_iterations = bootstrap_iterations,
      certainty_thresholds = certainty_thresholds,
      hull_type = hull_type,
      alpha_value = alpha_value,
      hull_buffer = hull_buffer,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      use_parallel = use_parallel,
      n_cores = n_cores,
      verbose = verbose
    )
    
    # Store results
    results[[pair_name]] <- pair_result
    
    # Extract summary metrics
    if (!is.null(pair_result$gap_metrics) && nrow(pair_result$gap_metrics) > 0) {
      summary_rows[[pair_name]] <- cbind(
        pc_pair = pair_name,
        pc_x = pc_x,
        pc_y = pc_y,
        pair_result$gap_metrics
      )
    }
  }
  
  # Combine summary table
  if (length(summary_rows) > 0) {
    summary_table <- do.call(rbind, summary_rows)
    rownames(summary_table) <- NULL
  } else {
    summary_table <- data.frame()
  }
  
  # Prepare output
  output <- list(
    pc_pairs = pc_pairs,
    results = results,
    summary_table = summary_table,
    parameters = parameters
  )
  
  class(output) <- c("morphospace_gaps", "list")
  
  if (verbose) {
    cat("\n=== Gap Detection Complete ===\n")
    cat(sprintf("Analyzed %d PC pairs\n", length(results)))
    cat(sprintf("Detected %d gap regions\n", nrow(summary_table)))
  }
  
  if (!is.null(progress_callback)) {
    progress_callback("Analysis complete!", 0.05)
  }
  
  return(output)
}


#' Analyze Single PC Pair for Gaps
#'
#' Internal function to perform gap analysis on one PC pair
#'
#' @keywords internal
.analyze_pc_pair <- function(points,
                             uncertainty,
                             grid_resolution,
                             monte_carlo_iterations,
                             bootstrap_iterations,
                             certainty_thresholds,
                             hull_type,
                             alpha_value,
                             hull_buffer,
                             uncertainty_type,
                             occupancy_method,
                             occupancy_radius,
                             use_parallel,
                             n_cores,
                             verbose) {
  
  n_points <- nrow(points)
  
  # Calculate axis ranges and uncertainty radii
  x_range <- range(points$x)
  y_range <- range(points$y)
  
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  
  u_x <- uncertainty * x_span
  u_y <- uncertainty * y_span
  
  if (verbose) {
    cat(sprintf("  Points: %d | X range: [%.3f, %.3f] | Y range: [%.3f, %.3f]\n",
                n_points, x_range[1], x_range[2], y_range[1], y_range[2]))
    cat(sprintf("  Uncertainty: ±%.3f (X), ±%.3f (Y)\n", u_x, u_y))
  }
  
  # Create analysis domain hull
  domain_hull <- .create_domain_hull(
    points = points,
    hull_type = hull_type,
    alpha_value = alpha_value,
    buffer = hull_buffer,
    verbose = verbose
  )
  
  # Create regular grid
  grid_obj <- .create_analysis_grid(
    x_range = x_range,
    y_range = y_range,
    resolution = grid_resolution,
    buffer = hull_buffer
  )
  
  grid_x <- grid_obj$grid_x
  grid_y <- grid_obj$grid_y
  cell_size <- grid_obj$cell_size
  
  # Filter grid cells within domain
  grid_centers <- expand.grid(x = grid_x, y = grid_y)
  grid_sf <- sf::st_as_sf(grid_centers, coords = c("x", "y"), crs = sf::st_crs(domain_hull))
  in_domain <- as.vector(sf::st_within(grid_sf, domain_hull, sparse = FALSE))
  
  if (verbose) {
    cat(sprintf("  Grid: %d × %d = %d cells (%d within domain)\n",
                length(grid_x), length(grid_y), 
                length(grid_x) * length(grid_y), sum(in_domain)))
  }
  
  # Stage 1: Monte Carlo simulation for measurement uncertainty
  if (verbose) {
    cat(sprintf("  Stage 1: Monte Carlo simulation (%d iterations)\n", 
                monte_carlo_iterations))
  }
  
  gap_probability <- .compute_gap_probability(
    points = points,
    grid_x = grid_x,
    grid_y = grid_y,
    u_x = u_x,
    u_y = u_y,
    in_domain = in_domain,
    monte_carlo_iterations = monte_carlo_iterations,
    uncertainty_type = uncertainty_type,
    occupancy_method = occupancy_method,
    occupancy_radius = occupancy_radius,
    cell_size = cell_size,
    use_parallel = use_parallel,
    n_cores = n_cores
  )
  
  # Stage 2: Bootstrap for sampling uncertainty
  if (verbose) {
    cat(sprintf("  Stage 2: Bootstrap resampling (%d iterations)\n", 
                bootstrap_iterations))
  }
  
  gap_stability <- .compute_gap_stability(
    points = points,
    grid_x = grid_x,
    grid_y = grid_y,
    u_x = u_x,
    u_y = u_y,
    in_domain = in_domain,
    bootstrap_iterations = bootstrap_iterations,
    monte_carlo_iterations = monte_carlo_iterations,
    uncertainty_type = uncertainty_type,
    occupancy_method = occupancy_method,
    occupancy_radius = occupancy_radius,
    cell_size = cell_size,
    use_parallel = use_parallel,
    n_cores = n_cores
  )
  
  # Combine certainty metrics
  gap_certainty <- gap_probability * gap_stability
  
  # Extract gap polygons at specified thresholds
  if (verbose) {
    cat("  Extracting gap polygons\n")
  }
  
  gap_results <- .extract_gap_polygons(
    gap_certainty = gap_certainty,
    grid_x = grid_x,
    grid_y = grid_y,
    in_domain = in_domain,
    certainty_thresholds = certainty_thresholds,
    domain_hull = domain_hull,
    points = points
  )
  
  # Compile results
  result <- list(
    grid_x = grid_x,
    grid_y = grid_y,
    gap_probability = gap_probability,
    gap_stability = gap_stability,
    gap_certainty = gap_certainty,
    gap_polygons = gap_results$polygons,
    gap_metrics = gap_results$metrics,
    domain_hull = domain_hull,
    cell_size = cell_size
  )
  
  return(result)
}


#' Create Domain Hull
#'
#' @keywords internal
.create_domain_hull <- function(points, hull_type, alpha_value, buffer, verbose) {
  
  if (hull_type == "alpha" && requireNamespace("alphahull", quietly = TRUE)) {
    
    # Determine alpha value if not provided
    if (is.null(alpha_value)) {
      # Auto-determine alpha: use a more inclusive approach
      # Calculate pairwise distances
      dists <- as.matrix(dist(points))
      diag(dists) <- NA
      
      # Strategy: Use maximum nearest neighbor distance (most inclusive)
      # This ensures even outlier points are included in the hull
      max_nn_dist <- max(apply(dists, 1, min, na.rm = TRUE))
      
      # Use 2x the max nearest neighbor distance for a generous hull
      # This captures the full distribution including sparse regions
      alpha_value <- 2 * max_nn_dist
      
      if (verbose) {
        cat(sprintf("  Alpha hull: auto alpha = %.4f (2x max nearest neighbor)\n", alpha_value))
      }
    }
    
    # Create alpha hull
    tryCatch({
      ahull <- alphahull::ashape(points, alpha = alpha_value)
      
      # Convert to sf polygon
      # Extract edges from alpha hull
      edges <- ahull$edges
      
      if (nrow(edges) == 0) {
        if (verbose) {
          cat("  Alpha hull failed (no edges). Using convex hull.\n")
        }
        hull_type <- "convex"
      } else {
        # Build polygon from edges (simplified approach)
        hull_coords <- unique(rbind(
          ahull$x[edges[, "ind1"], ],
          ahull$x[edges[, "ind2"], ]
        ))
        
        # Order points for polygon (use convex hull of alpha hull points)
        hull_idx <- grDevices::chull(hull_coords)
        hull_coords <- hull_coords[c(hull_idx, hull_idx[1]), ]
        
        hull_poly <- sf::st_polygon(list(hull_coords))
        hull_sf <- sf::st_sfc(hull_poly)
        hull_sf <- sf::st_sf(geometry = hull_sf, crs = NA)
      }
    }, error = function(e) {
      if (verbose) {
        cat(sprintf("  Alpha hull error: %s. Using convex hull.\n", e$message))
      }
      hull_type <<- "convex"
    })
  }
  
  # Fall back to convex hull
  if (hull_type == "convex" || !exists("hull_sf")) {
    hull_idx <- grDevices::chull(points$x, points$y)
    hull_coords <- as.matrix(points[c(hull_idx, hull_idx[1]), ])
    
    hull_poly <- sf::st_polygon(list(hull_coords))
    hull_sf <- sf::st_sfc(hull_poly)
    hull_sf <- sf::st_sf(geometry = hull_sf, crs = NA)
    
    if (verbose) {
      cat("  Using convex hull\n")
    }
  }
  
  # Apply buffer if requested
  if (buffer > 0) {
    # Calculate appropriate buffer distance
    x_span <- diff(range(points$x))
    y_span <- diff(range(points$y))
    buffer_dist <- buffer * mean(c(x_span, y_span))
    
    hull_sf <- sf::st_buffer(hull_sf, dist = buffer_dist)
    
    if (verbose) {
      cat(sprintf("  Applied %.1f%% buffer (%.3f units)\n", 
                  buffer * 100, buffer_dist))
    }
  }
  
  return(hull_sf)
}


#' Create Analysis Grid
#'
#' @keywords internal
.create_analysis_grid <- function(x_range, y_range, resolution, buffer) {
  
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  
  # Expand range slightly for buffer
  x_range_buffered <- x_range + c(-1, 1) * buffer * x_span
  y_range_buffered <- y_range + c(-1, 1) * buffer * y_span
  
  # Create grid
  grid_x <- seq(x_range_buffered[1], x_range_buffered[2], length.out = resolution)
  grid_y <- seq(y_range_buffered[1], y_range_buffered[2], length.out = resolution)
  
  # Calculate cell size (for occupancy radius)
  cell_size <- mean(c(diff(grid_x)[1], diff(grid_y)[1]))
  
  list(
    grid_x = grid_x,
    grid_y = grid_y,
    cell_size = cell_size
  )
}


#' Compute Gap Probability via Monte Carlo
#'
#' @keywords internal
.compute_gap_probability <- function(points,
                                     grid_x,
                                     grid_y,
                                     u_x,
                                     u_y,
                                     in_domain,
                                     monte_carlo_iterations,
                                     uncertainty_type,
                                     occupancy_method,
                                     occupancy_radius,
                                     cell_size,
                                     use_parallel,
                                     n_cores) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  n_points <- nrow(points)
  
  # Initialize occupancy counter
  occupancy_count <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  # Determine occupancy radius in actual units
  radius <- occupancy_radius * cell_size
  
  # Monte Carlo iterations
  if (use_parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel execution
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export necessary objects
    parallel::clusterExport(cl, 
                           varlist = c("points", "u_x", "u_y", 
                                      "uncertainty_type", "grid_x", "grid_y",
                                      "occupancy_method", "radius"),
                           envir = environment())
    
    occupancy_matrices <- parallel::parLapply(cl, 1:monte_carlo_iterations, function(iter) {
      .monte_carlo_iteration(points, u_x, u_y, uncertainty_type,
                            grid_x, grid_y, occupancy_method, radius)
    })
    
    # Sum occupancy matrices
    for (m in occupancy_matrices) {
      occupancy_count <- occupancy_count + m
    }
    
  } else {
    # Sequential execution
    for (iter in 1:monte_carlo_iterations) {
      occupancy_matrix <- .monte_carlo_iteration(
        points, u_x, u_y, uncertainty_type,
        grid_x, grid_y, occupancy_method, radius
      )
      occupancy_count <- occupancy_count + occupancy_matrix
    }
  }
  
  # Calculate occupancy probability
  occupancy_prob <- occupancy_count / monte_carlo_iterations
  
  # Gap probability = 1 - occupancy probability
  gap_prob <- 1 - occupancy_prob
  
  # Mask cells outside domain
  gap_prob_matrix <- matrix(gap_prob, nrow = n_grid_x, ncol = n_grid_y)
  in_domain_matrix <- matrix(in_domain, nrow = n_grid_x, ncol = n_grid_y)
  gap_prob_matrix[!in_domain_matrix] <- NA
  
  return(gap_prob_matrix)
}


#' Single Monte Carlo Iteration
#'
#' @keywords internal
.monte_carlo_iteration <- function(points, u_x, u_y, uncertainty_type,
                                   grid_x, grid_y, occupancy_method, radius) {
  
  n_points <- nrow(points)
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  
  # Perturb points according to uncertainty model
  if (uncertainty_type == "gaussian") {
    # Gaussian: σ chosen so ±u ≈ 95% interval (u = 1.96 * σ)
    sigma_x <- u_x / 1.96
    sigma_y <- u_y / 1.96
    
    perturbed_x <- points$x + rnorm(n_points, mean = 0, sd = sigma_x)
    perturbed_y <- points$y + rnorm(n_points, mean = 0, sd = sigma_y)
    
  } else if (uncertainty_type == "uniform") {
    # Uniform within ±u
    perturbed_x <- points$x + runif(n_points, min = -u_x, max = u_x)
    perturbed_y <- points$y + runif(n_points, min = -u_y, max = u_y)
    
  } else {
    stop("Unknown uncertainty_type. Use 'gaussian' or 'uniform'.")
  }
  
  perturbed_points <- data.frame(x = perturbed_x, y = perturbed_y)
  
  # Determine occupancy for each grid cell
  occupancy <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  if (occupancy_method == "radius") {
    # For each grid cell, check if any point is within radius
    grid_centers <- expand.grid(x = grid_x, y = grid_y)
    
    for (j in seq_len(nrow(perturbed_points))) {
      # Calculate distances from this point to all grid centers
      dx <- grid_centers$x - perturbed_points$x[j]
      dy <- grid_centers$y - perturbed_points$y[j]
      dists <- sqrt(dx^2 + dy^2)
      
      # Mark cells within radius as occupied
      occupied_cells <- which(dists <= radius)
      if (length(occupied_cells) > 0) {
        # Convert linear index to matrix indices
        row_idx <- ((occupied_cells - 1) %% n_grid_x) + 1
        col_idx <- ((occupied_cells - 1) %/% n_grid_x) + 1
        
        for (k in seq_along(occupied_cells)) {
          occupancy[row_idx[k], col_idx[k]] <- 1
        }
      }
    }
    
  } else if (occupancy_method == "kde") {
    # Kernel density estimation approach
    if (requireNamespace("MASS", quietly = TRUE)) {
      kde <- MASS::kde2d(perturbed_points$x, perturbed_points$y,
                        n = c(n_grid_x, n_grid_y),
                        lims = c(range(grid_x), range(grid_y)))
      
      # Threshold: cells with density > 0 are occupied
      occupancy <- ifelse(kde$z > 0, 1, 0)
    } else {
      # Fall back to radius method
      warning("MASS package not available. Using radius method instead of KDE.")
      return(.monte_carlo_iteration(points, u_x, u_y, uncertainty_type,
                                    grid_x, grid_y, "radius", radius))
    }
  }
  
  return(occupancy)
}


#' Compute Gap Stability via Bootstrap
#'
#' @keywords internal
.compute_gap_stability <- function(points,
                                   grid_x,
                                   grid_y,
                                   u_x,
                                   u_y,
                                   in_domain,
                                   bootstrap_iterations,
                                   monte_carlo_iterations,
                                   uncertainty_type,
                                   occupancy_method,
                                   occupancy_radius,
                                   cell_size,
                                   use_parallel,
                                   n_cores) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  n_points <- nrow(points)
  
  # Initialize gap classification counter
  gap_count <- matrix(0, nrow = n_grid_x, ncol = n_grid_y)
  
  # Choose gap probability threshold for binary classification
  gap_threshold <- 0.5  # Cells with gap_prob > 0.5 are classified as gaps
  
  # Bootstrap iterations
  for (b in 1:bootstrap_iterations) {
    # Resample points with replacement
    boot_indices <- sample(1:n_points, size = n_points, replace = TRUE)
    boot_points <- points[boot_indices, ]
    
    # Compute gap probability for this bootstrap sample
    # (using reduced Monte Carlo iterations for efficiency)
    mc_iter_boot <- max(50, monte_carlo_iterations %/% 2)
    
    gap_prob_boot <- .compute_gap_probability(
      points = boot_points,
      grid_x = grid_x,
      grid_y = grid_y,
      u_x = u_x,
      u_y = u_y,
      in_domain = in_domain,
      monte_carlo_iterations = mc_iter_boot,
      uncertainty_type = uncertainty_type,
      occupancy_method = occupancy_method,
      occupancy_radius = occupancy_radius,
      cell_size = cell_size,
      use_parallel = FALSE,  # Already in bootstrap loop
      n_cores = 1
    )
    
    # Create binary gap mask
    is_gap <- gap_prob_boot > gap_threshold
    is_gap[is.na(is_gap)] <- FALSE
    
    # Increment counter for cells classified as gaps
    gap_count <- gap_count + is_gap
  }
  
  # Calculate stability: proportion of bootstrap iterations where classified as gap
  gap_stability <- gap_count / bootstrap_iterations
  
  # Mask cells outside domain
  in_domain_matrix <- matrix(in_domain, nrow = n_grid_x, ncol = n_grid_y)
  gap_stability[!in_domain_matrix] <- NA
  
  return(gap_stability)
}


#' Extract Gap Polygons from Certainty Matrix
#'
#' @keywords internal
.extract_gap_polygons <- function(gap_certainty,
                                  grid_x,
                                  grid_y,
                                  in_domain,
                                  certainty_thresholds,
                                  domain_hull,
                                  points) {
  
  n_grid_x <- length(grid_x)
  n_grid_y <- length(grid_y)
  
  all_polygons <- list()
  all_metrics <- list()
  
  for (threshold in certainty_thresholds) {
    
    # Create binary mask for this threshold
    is_gap <- gap_certainty >= threshold
    is_gap[is.na(is_gap)] <- FALSE
    
    if (sum(is_gap) == 0) {
      next  # No gaps at this threshold
    }
    
    # Convert to raster-like structure for contouring
    # Create data frame of gap cells
    gap_cells <- which(is_gap, arr.ind = TRUE)
    
    if (nrow(gap_cells) == 0) {
      next
    }
    
    # Get coordinates of gap cells
    gap_coords <- data.frame(
      x = grid_x[gap_cells[, 1]],
      y = grid_y[gap_cells[, 2]],
      certainty = gap_certainty[gap_cells]
    )
    
    # Create point geometry
    gap_sf <- sf::st_as_sf(gap_coords, coords = c("x", "y"), crs = NA)
    
    # Buffer points to create polygons, then union
    cell_size <- mean(c(diff(grid_x)[1], diff(grid_y)[1]))
    buffer_dist <- cell_size * 0.5
    
    gap_buffered <- sf::st_buffer(gap_sf, dist = buffer_dist)
    gap_union <- sf::st_union(gap_buffered)
    
    # Cast to multipolygon then extract individual polygons
    gap_multi <- sf::st_cast(gap_union, "MULTIPOLYGON")
    gap_polys <- sf::st_cast(gap_multi, "POLYGON")
    
    # Convert to sf data frame
    gap_polys_sf <- sf::st_sf(geometry = gap_polys, crs = NA)
    
    # Intersect with domain hull
    gap_polys_sf <- sf::st_intersection(gap_polys_sf, domain_hull)
    
    if (nrow(gap_polys_sf) == 0) {
      next
    }
    
    # Smooth the polygons to remove grid artifacts
    # Use a tolerance based on grid cell size
    smooth_tolerance <- cell_size * 0.3
    gap_polys_sf <- sf::st_simplify(gap_polys_sf, dTolerance = smooth_tolerance, preserveTopology = TRUE)
    
    # Apply additional smoothing using a small buffer trick (erosion-dilation)
    # This helps smooth out the stepped edges
    smooth_buffer <- cell_size * 0.1
    gap_polys_sf <- sf::st_buffer(gap_polys_sf, dist = -smooth_buffer)  # Erode
    gap_polys_sf <- sf::st_buffer(gap_polys_sf, dist = smooth_buffer)   # Dilate back
    
    # Remove any empty geometries from the smoothing
    gap_polys_sf <- gap_polys_sf[!sf::st_is_empty(gap_polys_sf), ]
    
    if (nrow(gap_polys_sf) == 0) {
      next
    }
    
    # Calculate metrics for each gap polygon
    metrics <- data.frame(
      gap_id = seq_len(nrow(gap_polys_sf)),
      threshold = threshold,
      area = as.numeric(sf::st_area(gap_polys_sf)),
      stringsAsFactors = FALSE
    )
    
    # Calculate mean and max certainty
    metrics$mean_certainty <- NA
    metrics$max_certainty <- NA
    
    for (i in seq_len(nrow(gap_polys_sf))) {
      # Find grid cells within this polygon
      poly <- gap_polys_sf[i, ]
      grid_centers <- expand.grid(x = grid_x, y = grid_y)
      grid_centers_sf <- sf::st_as_sf(grid_centers, coords = c("x", "y"), crs = NA)
      
      in_poly <- as.vector(sf::st_within(grid_centers_sf, poly, sparse = FALSE))
      
      if (sum(in_poly) > 0) {
        certainty_values <- gap_certainty[matrix(in_poly, nrow = n_grid_x, ncol = n_grid_y)]
        certainty_values <- certainty_values[!is.na(certainty_values)]
        
        if (length(certainty_values) > 0) {
          metrics$mean_certainty[i] <- mean(certainty_values)
          metrics$max_certainty[i] <- max(certainty_values)
        }
      }
    }
    
    # Calculate centroid
    centroids <- sf::st_centroid(gap_polys_sf)
    centroid_coords <- sf::st_coordinates(centroids)
    metrics$centroid_x <- centroid_coords[, 1]
    metrics$centroid_y <- centroid_coords[, 2]
    
    # Calculate gap depth (distance to nearest occupied point)
    metrics$gap_depth <- NA
    
    for (i in seq_len(nrow(gap_polys_sf))) {
      centroid <- c(metrics$centroid_x[i], metrics$centroid_y[i])
      dists <- sqrt((points$x - centroid[1])^2 + (points$y - centroid[2])^2)
      metrics$gap_depth[i] <- min(dists)
    }
    
    # Add threshold-specific results
    gap_polys_sf$threshold <- threshold
    all_polygons[[as.character(threshold)]] <- gap_polys_sf
    all_metrics[[as.character(threshold)]] <- metrics
  }
  
  # Combine results
  if (length(all_polygons) > 0) {
    polygons <- do.call(rbind, all_polygons)
    metrics <- do.call(rbind, all_metrics)
    rownames(metrics) <- NULL
  } else {
    polygons <- sf::st_sf(geometry = sf::st_sfc(), crs = NA)
    metrics <- data.frame()
  }
  
  list(
    polygons = polygons,
    metrics = metrics
  )
}


#' Print Method for morphospace_gaps
#'
#' @param x Object of class morphospace_gaps
#' @param ... Additional arguments (ignored)
#'
#' @export
print.morphospace_gaps <- function(x, ...) {
  cat("Morphospace Gap Detection Results\n")
  cat("==================================\n\n")
  
  cat(sprintf("PC Pairs Analyzed: %d\n", nrow(x$pc_pairs)))
  cat(sprintf("Total Gaps Detected: %d\n", nrow(x$summary_table)))
  
  cat("\nParameters:\n")
  cat(sprintf("  Uncertainty: %.1f%%\n", x$parameters$uncertainty * 100))
  cat(sprintf("  Grid Resolution: %d\n", x$parameters$grid_resolution))
  cat(sprintf("  Monte Carlo Iterations: %d\n", x$parameters$monte_carlo_iterations))
  cat(sprintf("  Bootstrap Iterations: %d\n", x$parameters$bootstrap_iterations))
  cat(sprintf("  Hull Type: %s\n", x$parameters$hull_type))
  
  if (nrow(x$summary_table) > 0) {
    cat("\nTop Gaps by Area:\n")
    top_gaps <- head(x$summary_table[order(-x$summary_table$area), ], 5)
    print(top_gaps[, c("pc_pair", "threshold", "area", "mean_certainty", "gap_depth")])
  }
  
  invisible(x)
}
