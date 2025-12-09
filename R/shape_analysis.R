#' Analyze Shape Images and Perform PCA
#'
#' This function processes a directory of shape images, normalizes the shapes,
#' performs Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA),
#' displays a plot of PC contributions, and saves PCA scores to an Excel file.
#'
#' @param shape_dir A character string specifying the directory containing shape files
#'   (images in JPG format). The directory must exist and contain at least one valid JPG file.
#' @param norm Logical. Controls EFA normalization. If TRUE, Fourier descriptors are
#'   normalized to the first harmonic ("align by first harmonic"). If FALSE, they are
#'   normalized to the longest radius. Shapes are always centered, scaled, and slid to
#'   the specified start direction prior to EFA. Default is TRUE.
#' @param output_dir A character string specifying the directory where the Excel file will be saved.
#'   If NULL, the file will be saved in the working directory.
#' @param output_file A character string specifying the name of the Excel file where PCA scores will
#'   be saved. Default is "shape_analysis.xlsx".
#' @param num_pcs Integer. The number of principal components to display in the PC contribution plot.
#'   Must be between 1 and 50. Default is 10.
#' @param start_point A character string specifying the starting point for alignment during shape
#'   normalization. Options are "up", "left", "down", or "right". Default is "left".
#' @param harmonics Integer. Number of harmonics to use for EFA. Default is NULL (automatic).
#' @param verbose Logical. Should progress messages be printed? Default is TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{pca_results}{The PCA object from Momocs}
#'     \item{efa_results}{The EFA coefficients object from Momocs}
#'     \item{scores}{Data frame of PCA scores}
#'     \item{summary}{Character string of PCA summary}
#'     \item{output_path}{Path where Excel results were saved}
#'     \item{summary_txt_path}{Path to saved PCA summary .txt file}
#'     \item{pc_plot_jpg_path}{Path to saved PC contributions .jpg}
#'     \item{reconstruction_model_path}{Path to RDS file containing reconstruction model}
#'     \item{reconstruction_info_path}{Path to text file with reconstruction info}
#'     \item{metadata}{List of analysis metadata (n_shapes, harmonics, etc.)}
#'   }
#'
#' @details
#' The function uses the `Momocs` package to import, normalize, and analyze shape outlines:
#' - **Import**: Reads JPG/JPEG files and converts to shape outlines
#' - **Pre-processing**: Centers, scales, and slides shapes to a consistent start
#'   direction (up/left/down/right)
#' - **Normalization**: EFA descriptors normalized to first harmonic when `norm = TRUE`,
#'   or to the longest radius when `norm = FALSE`
#' - **Elliptical Fourier Analysis (EFA)**: Generates Fourier descriptors for the shape outlines
#' - **Principal Component Analysis (PCA)**: Decomposes Fourier descriptors into principal components
#'
#' PCA scores for all analyzed shapes are saved to an Excel file. A plot of the contributions of the
#' specified number of principal components is displayed.
#'
#' @section Dependencies:
#' This function requires the following R packages:
#' - \pkg{Momocs} for shape analysis
#' - \pkg{openxlsx} for writing PCA scores to an Excel file
#'
#' @examples
#' \dontrun{
#' # Basic shape analysis
#' result <- shape_analysis(
#'   shape_dir = "path/to/shape/directory",
#'   output_file = "my_analysis.xlsx"
#' )
#'
#' # Advanced analysis with custom parameters
#' result <- shape_analysis(
#'   shape_dir = "path/to/shape/directory",
#'   norm = TRUE,
#'   output_dir = "path/to/output/directory",
#'   output_file = "shape_analysis_results.xlsx",
#'   num_pcs = 5,
#'   start_point = "down",
#'   harmonics = 20,
#'   verbose = TRUE
#' )
#'
#' # Access results
#' print(result$summary)
#' head(result$scores)
#' }
#'
#' @export
shape_analysis <- function(shape_dir,
                          norm = TRUE,
                          output_dir = NULL,
                          output_file = "shape_analysis.xlsx",
                          num_pcs = 10,
                          start_point = "left",
                          harmonics = NULL,
                          align_orientation = FALSE,
                          verbose = TRUE) {
  
  # Input validation ----
  .validate_shape_analysis_inputs(
    shape_dir, norm, output_dir, output_file, 
    num_pcs, start_point, harmonics, verbose
  )
  
  # Setup output directory ----
  output_dir <- .setup_output_directory(output_dir, verbose)
  output_file_path <- file.path(output_dir, output_file)
  
  # Import and validate shapes ----
  if (verbose) message("Importing shape files from: ", shape_dir)
  
  shapes <- .import_shape_files(shape_dir, verbose)
  
  # Always pre-process: center -> scale -> slide in user-selected direction
  if (verbose) message("Pre-processing shapes: center -> scale -> slide '", start_point, "'")
  normalized_shapes <- .normalize_shapes(shapes, start_point, align_orientation, verbose)
  
  # Perform Elliptical Fourier Analysis ----
  if (verbose) message("Performing Elliptical Fourier Analysis...")
  # 'norm' controls whether descriptors are normalized to first harmonic (TRUE) or longest radius (FALSE)
  # Use start = TRUE to set a consistent phasing across shapes.
  
  # If normalization is enabled, we need to capture size/position info for reconstruction
  shape_metadata <- NULL
  if (norm) {
    if (verbose) message("Extracting shape metadata for denormalization...")
    # Extract centroid size and position for each shape before EFA normalization
    shape_metadata <- list()
    for (i in seq_along(normalized_shapes$coo)) {
      coords <- normalized_shapes$coo[[i]]
      # Calculate centroid
      centroid <- colMeans(coords)
      # Calculate size (mean distance from centroid)
      size <- mean(sqrt(rowSums((coords - rep(centroid, each = nrow(coords)))^2)))
      shape_metadata[[i]] <- list(
        centroid = centroid,
        size = size,
        name = names(normalized_shapes$coo)[i]
      )
    }
    if (verbose) message("  Captured metadata for ", length(shape_metadata), " shapes")
  }
  
  efa_results <- .perform_efa(normalized_shapes, norm = norm, harmonics = harmonics, start = TRUE, verbose = verbose)
  
  # Perform PCA ----
  if (verbose) message("Performing Principal Component Analysis...")
  # Match manual script default: center = TRUE, scale. = FALSE (and keep fallback behavior if needed)
  # We'll directly run with scale.=FALSE to be consistent; robust fallback no longer needed.
  pca_results <- tryCatch({
    Momocs::PCA(efa_results, center = TRUE, scale. = FALSE)
  }, error = function(e) {
    stop("Principal Component Analysis failed: ", conditionMessage(e), call. = FALSE)
  })
  
  # Store efa_results for reconstruction (keeping reference before any cleanup)
  efa_results_stored <- efa_results
  
  # Debug: Check what's in EFA results for reconstruction
  if (verbose) {
    message("EFA results structure for reconstruction:")
    message("  Components: ", paste(names(efa_results), collapse = ", "))
    message("  norm attribute: ", if (!is.null(efa_results$norm)) efa_results$norm else "NULL")
    message("  r1 present: ", !is.null(efa_results$r1))
    message("  r2 present: ", !is.null(efa_results$r2))
    message("  baseline1 present: ", !is.null(efa_results$baseline1))
    message("  baseline2 present: ", !is.null(efa_results$baseline2))
    message("  method attribute: ", if (!is.null(attr(efa_results, "method"))) attr(efa_results, "method") else "NULL")
  }
  
  # Extract and format results ----
  if (verbose) message("Extracting PCA scores...")
  scores <- .extract_pca_scores(pca_results)
  
  # Save results to Excel ----
  if (verbose) message("Saving results to: ", output_file_path)
  .save_results_to_excel(scores, output_file_path)
  
  # Generate summary and plots ----
  if (verbose) message("Generating summary and plots...")
  analysis_summary <- .generate_pca_summary(pca_results)
  
  # Create PC contribution plot
  pc_plot <- .create_pc_contribution_plot(pca_results, num_pcs)

  # Save additional artifacts (TXT summary and JPG plot) ----
  # Build output file paths based on Excel file name
  output_stem <- tools::file_path_sans_ext(basename(output_file_path))
  summary_txt_path <- file.path(output_dir, paste0(output_stem, "_summary.txt"))
  pc_plot_jpg_path <- file.path(output_dir, paste0(output_stem, "_pc_contrib.jpg"))

  # Write summary text
  tryCatch({
    writeLines(analysis_summary, con = summary_txt_path, useBytes = TRUE)
  }, error = function(e) {
    warning("Failed to write summary TXT: ", conditionMessage(e))
  })

  # Save plot as JPEG (regenerate if needed)
  tryCatch({
    grDevices::jpeg(filename = pc_plot_jpg_path, width = 1600, height = 1200, quality = 95)
    if (!is.null(pc_plot)) {
      print(pc_plot)
    } else {
      max_pcs <- min(num_pcs, ncol(pca_results$x))
      print(Momocs::PCcontrib(pca_results, nax = 1:max_pcs, sd.r = c(-2,-1,0,1,2)))
    }
    grDevices::dev.off()
  }, error = function(e) {
    grDevices::dev.off()
    warning("Failed to save PC contribution plot JPG: ", conditionMessage(e))
  })
  
  # Save reconstruction data ----
  reconstruction_model_path <- file.path(output_dir, paste0(output_stem, "_reconstruction_model.rds"))
  reconstruction_info_path <- file.path(output_dir, paste0(output_stem, "_reconstruction_info.txt"))
  
  if (verbose) message("Saving reconstruction model data...")
  .save_reconstruction_data(
    pca_results = pca_results,
    efa_results = efa_results_stored,
    shape_metadata = shape_metadata,
    norm = norm,
    harmonics = harmonics,
    start_point = start_point,
    model_path = reconstruction_model_path,
    info_path = reconstruction_info_path,
    verbose = verbose
  )
  
  # Prepare metadata
  metadata <- list(
    n_shapes = length(shapes),
    n_harmonics = ifelse(is.null(harmonics), "automatic", harmonics),
    normalization = norm,
    used_raw_shapes = FALSE,
    start_point = start_point,
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Analysis completed successfully!")
  
  # Return comprehensive results ----
  structure(
    list(
      pca_results = pca_results,
      efa_results = efa_results_stored,
      scores = scores,
      summary = analysis_summary,
      output_path = output_file_path,
      summary_txt_path = summary_txt_path,
      pc_plot_jpg_path = pc_plot_jpg_path,
      reconstruction_model_path = reconstruction_model_path,
      reconstruction_info_path = reconstruction_info_path,
      metadata = metadata,
      pc_contribution_plot = pc_plot
    ),
    class = "shape_analysis_result"
  )
}

# Helper Functions ----

#' Validate inputs for shape_analysis function
#' @noRd
.validate_shape_analysis_inputs <- function(shape_dir, norm, output_dir, output_file,
                                          num_pcs, start_point, harmonics, verbose) {
  # Check required packages
  if (!requireNamespace("Momocs", quietly = TRUE)) {
    stop("Package 'Momocs' is required but not installed. Please install it with: install.packages('Momocs')",
         call. = FALSE)
  }
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required but not installed. Please install it with: install.packages('openxlsx')",
         call. = FALSE)
  }
  
  # Validate shape_dir
  if (!is.character(shape_dir) || length(shape_dir) != 1 || nchar(shape_dir) == 0) {
    stop("'shape_dir' must be a non-empty character string", call. = FALSE)
  }
  if (!dir.exists(shape_dir)) {
    stop("The specified shape directory does not exist: ", shape_dir, call. = FALSE)
  }
  
  # Validate norm
  if (!is.logical(norm) || length(norm) != 1) {
    stop("'norm' must be a single logical value", call. = FALSE)
  }
  
  # Validate output_dir
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1 || nchar(output_dir) == 0) {
      stop("'output_dir' must be NULL or a non-empty character string", call. = FALSE)
    }
  }
  
  # Validate output_file
  if (!is.character(output_file) || length(output_file) != 1 || nchar(output_file) == 0) {
    stop("'output_file' must be a non-empty character string", call. = FALSE)
  }
  if (!grepl("\\.(xlsx|xls)$", output_file, ignore.case = TRUE)) {
    stop("'output_file' must have .xlsx or .xls extension", call. = FALSE)
  }
  
  # Validate num_pcs
  if (!is.numeric(num_pcs) || length(num_pcs) != 1 || num_pcs < 1 || num_pcs > 50 || num_pcs != as.integer(num_pcs)) {
    stop("'num_pcs' must be an integer between 1 and 50", call. = FALSE)
  }
  
  # Validate start_point
  valid_start_points <- c("up", "left", "down", "right")
  if (!is.character(start_point) || length(start_point) != 1 || !start_point %in% valid_start_points) {
    stop("'start_point' must be one of: ", paste(valid_start_points, collapse = ", "), call. = FALSE)
  }
  
  # Validate harmonics
  if (!is.null(harmonics)) {
    if (!is.numeric(harmonics) || length(harmonics) != 1 || harmonics < 1 || harmonics > 100 || harmonics != as.integer(harmonics)) {
      stop("'harmonics' must be NULL or an integer between 1 and 100", call. = FALSE)
    }
  }
  
  # Validate verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

#' Setup output directory
#' @noRd
.setup_output_directory <- function(output_dir, verbose) {
  if (is.null(output_dir)) {
    output_dir <- getwd()
    if (verbose) message("Using current working directory for output: ", output_dir)
  } else {
    if (!dir.exists(output_dir)) {
      stop("The specified output directory does not exist: ", output_dir, call. = FALSE)
    }
  }
  return(output_dir)
}

#' Import shape files from directory
#' @noRd
.import_shape_files <- function(shape_dir, verbose) {
  # Get list of shape files
  shape_files <- list.files(
    shape_dir, 
    pattern = "\\.(jpg|jpeg)$", 
    full.names = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(shape_files) == 0) {
    stop("No JPG/JPEG files found in the specified directory: ", shape_dir, call. = FALSE)
  }
  
  if (verbose) message("Found ", length(shape_files), " shape files")
  
  # Import shapes
  tryCatch({
    shapes <- Momocs::import_jpg(shape_files) %>% Momocs::Out()
    if (verbose) message("Successfully imported ", length(shapes), " shapes")
    return(shapes)
  }, error = function(e) {
    stop("Failed to import shape files: ", e$message, call. = FALSE)
  })
}

#' Normalize shapes
#' @noRd
.normalize_shapes <- function(shapes, start_point, align_orientation, verbose) {
  tryCatch({
    normalized_shapes <- shapes %>%
      Momocs::coo_center() %>%
      Momocs::coo_scale()
    if (isTRUE(align_orientation)) {
      normalized_shapes <- normalized_shapes %>% Momocs::coo_aligncalliper()
      if (verbose) message("Orientation aligned using major axis (caliper alignment)")
    }
    normalized_shapes <- normalized_shapes %>% Momocs::coo_slidedirection(start_point)
    
    if (verbose) message("Shapes normalized with start point: ", start_point)
    return(normalized_shapes)
  }, error = function(e) {
    stop("Failed to normalize shapes: ", e$message, call. = FALSE)
  })
}

#' Perform Elliptical Fourier Analysis
#' @noRd
.perform_efa <- function(normalized_shapes, norm, harmonics, start = NULL, verbose) {
  tryCatch({
    # Build argument list to support optional 'start' parameter
    args <- list(x = normalized_shapes, norm = norm)
    if (!is.null(harmonics)) args$nb.h <- harmonics
    if (!is.null(start)) args$start <- start
    efa_results <- do.call(Momocs::efourier, args)
    
    if (verbose) message("EFA completed with normalization: ", norm)
    return(efa_results)
  }, error = function(e) {
    stop("Elliptical Fourier Analysis failed: ", e$message, call. = FALSE)
  })
}

#' Perform Principal Component Analysis
#' @noRd
# No longer used: PCA is invoked directly with scale. = FALSE to match manual pipeline

#' Extract PCA scores as data frame
#' @noRd
.extract_pca_scores <- function(pca_results) {
  tryCatch({
    scores <- as.data.frame(pca_results$x)
    scores$ID <- rownames(scores)
    scores <- scores[, c("ID", setdiff(names(scores), "ID"))]
    return(scores)
  }, error = function(e) {
    stop("Failed to extract PCA scores: ", e$message, call. = FALSE)
  })
}

#' Save results to Excel file
#' @noRd
.save_results_to_excel <- function(scores, output_file_path) {
  tryCatch({
    openxlsx::write.xlsx(scores, file = output_file_path, rowNames = FALSE)
  }, error = function(e) {
    stop("Failed to write Excel file to ", output_file_path, ": ", e$message, call. = FALSE)
  })
}

#' Save reconstruction data for shape reconstruction
#' @noRd
.save_reconstruction_data <- function(pca_results, efa_results, shape_metadata, norm, harmonics, 
                                     start_point, model_path, info_path, verbose) {
  
  # Extract reconstruction components from PCA
  reconstruction_model <- list(
    # PCA components
    rotation = pca_results$rotation,      # Eigenvectors (loadings)
    center = pca_results$center,          # Centering values
    sdev = pca_results$sdev,              # Standard deviations
    
    # EFA results - save the COMPLETE OutCoe object for proper reconstruction
    efa_object = efa_results,             # Complete EFA object with all attributes
    
    # Shape metadata for denormalization (our manual capture - backup method)
    shape_metadata = shape_metadata,
    
    # Analysis parameters
    parameters = list(
      norm = norm,
      harmonics = if (is.null(harmonics)) "automatic" else harmonics,
      start_point = start_point,
      n_components = ncol(pca_results$x),
      n_specimens = nrow(pca_results$x),
      n_harmonics_used = if (!is.null(efa_results$coe)) ncol(efa_results$coe) / 4 else NULL
    ),
    
    # Variance explained
    variance_explained = if (!is.null(pca_results$eig)) {
      pca_results$eig / sum(pca_results$eig) * 100
    } else {
      NULL
    },
    
    # Metadata for version control and validation
    metadata = list(
      format_version = "1.0",
      created_date = Sys.time(),
      momocs_version = tryCatch(as.character(packageVersion("Momocs")), error = function(e) "unknown"),
      r_version = R.version.string
    )
  )
  
  # Save as RDS (binary, preserves R objects exactly)
  tryCatch({
    saveRDS(reconstruction_model, file = model_path)
    if (verbose) message("  Reconstruction model saved: ", basename(model_path))
  }, error = function(e) {
    warning("Failed to save reconstruction model RDS: ", e$message)
  })
  
  # Create human-readable text summary
  info_lines <- c(
    "Shape Reconstruction Model Information",
    "======================================",
    "",
    "Created:", format(reconstruction_model$metadata$created_date),
    "Format Version:", reconstruction_model$metadata$format_version,
    "Momocs Version:", reconstruction_model$metadata$momocs_version,
    "R Version:", reconstruction_model$metadata$r_version,
    "",
    "Analysis Parameters:",
    "--------------------",
    paste("  Normalization:", reconstruction_model$parameters$norm),
    paste("  Harmonics:", reconstruction_model$parameters$harmonics),
    paste("  Start Point:", reconstruction_model$parameters$start_point),
    paste("  Number of Specimens:", reconstruction_model$parameters$n_specimens),
    paste("  Number of PCs:", reconstruction_model$parameters$n_components),
    "",
    "Reconstruction Components:",
    "-------------------------",
    paste("  Eigenvectors (rotation):", nrow(reconstruction_model$rotation), "x", ncol(reconstruction_model$rotation)),
    paste("  Center vector length:", length(reconstruction_model$center)),
    paste("  Standard deviations:", length(reconstruction_model$sdev)),
    "",
    "Variance Explained by PC:",
    "------------------------"
  )
  
  # Add variance explained for first 10 PCs
  if (!is.null(reconstruction_model$variance_explained)) {
    n_show <- min(10, length(reconstruction_model$variance_explained))
    for (i in 1:n_show) {
      info_lines <- c(info_lines, sprintf("  PC%d: %.2f%%", i, reconstruction_model$variance_explained[i]))
    }
    cumsum_var <- cumsum(reconstruction_model$variance_explained)
    info_lines <- c(info_lines, "", 
                   sprintf("  Cumulative variance (first 10 PCs): %.2f%%", cumsum_var[min(10, length(cumsum_var))]))
  }
  
  info_lines <- c(info_lines, "",
    "Usage:",
    "------",
    "To load this reconstruction model in R:",
    "  model <- load_reconstruction_model('" %+% basename(model_path) %+% "')",
    "",
    "To reconstruct shapes from PC scores:",
    "  reconstructed_shape <- reconstruct_shape_from_pca(",
    "    reconstruction_model = model,",
    "    pc_scores = c(PC1 = 2.0, PC2 = -1.5, ...)",
    "  )"
  )
  
  # Define simple string concatenation operator for cleaner code
  `%+%` <- function(a, b) paste0(a, b)
  
  # Recreate info_lines with proper concatenation
  info_lines <- c(
    "Shape Reconstruction Model Information",
    "======================================",
    "",
    paste("Created:", format(reconstruction_model$metadata$created_date)),
    paste("Format Version:", reconstruction_model$metadata$format_version),
    paste("Momocs Version:", reconstruction_model$metadata$momocs_version),
    paste("R Version:", reconstruction_model$metadata$r_version),
    "",
    "Analysis Parameters:",
    "--------------------",
    paste("  Normalization:", reconstruction_model$parameters$norm),
    paste("  Harmonics:", reconstruction_model$parameters$harmonics),
    paste("  Start Point:", reconstruction_model$parameters$start_point),
    paste("  Number of Specimens:", reconstruction_model$parameters$n_specimens),
    paste("  Number of PCs:", reconstruction_model$parameters$n_components),
    "",
    "Reconstruction Components:",
    "-------------------------",
    paste("  Eigenvectors (rotation):", nrow(reconstruction_model$rotation), "x", ncol(reconstruction_model$rotation)),
    paste("  Center vector length:", length(reconstruction_model$center)),
    paste("  Standard deviations:", length(reconstruction_model$sdev)),
    "",
    "Variance Explained by PC:",
    "------------------------"
  )
  
  if (!is.null(reconstruction_model$variance_explained)) {
    n_show <- min(10, length(reconstruction_model$variance_explained))
    for (i in 1:n_show) {
      info_lines <- c(info_lines, sprintf("  PC%d: %.2f%%", i, reconstruction_model$variance_explained[i]))
    }
    cumsum_var <- cumsum(reconstruction_model$variance_explained)
    info_lines <- c(info_lines, "", 
                   sprintf("  Cumulative variance (first 10 PCs): %.2f%%", cumsum_var[min(10, length(cumsum_var))]))
  }
  
  info_lines <- c(info_lines, "",
    "Usage:",
    "------",
    "To load this reconstruction model in R:",
    paste0("  model <- load_reconstruction_model('", basename(model_path), "')"),
    "",
    "To reconstruct shapes from PC scores:",
    "  reconstructed_shape <- reconstruct_shape_from_pca(",
    "    reconstruction_model = model,",
    "    pc_scores = c(PC1 = 2.0, PC2 = -1.5, ...)",
    "  )"
  )
  
  # Save text file
  tryCatch({
    writeLines(info_lines, con = info_path, useBytes = TRUE)
    if (verbose) message("  Reconstruction info saved: ", basename(info_path))
  }, error = function(e) {
    warning("Failed to save reconstruction info text: ", e$message)
  })
  
  invisible(TRUE)
}

#' Generate PCA summary
#' @noRd
.generate_pca_summary <- function(pca_results) {
  tryCatch({
    pca_summary <- capture.output(summary(pca_results))
    return(paste(pca_summary, collapse = "\n"))
  }, error = function(e) {
    warning("Failed to generate PCA summary: ", e$message)
    return("PCA summary generation failed")
  })
}

#' Create PC contribution plot
#' @noRd
.create_pc_contribution_plot <- function(pca_results, num_pcs) {
  tryCatch({
    # Ensure we don't request more PCs than available
    max_pcs <- min(num_pcs, ncol(pca_results$x))
    Momocs::PCcontrib(pca_results, nax = 1:max_pcs, sd.r = c(-2, -1, 0, 1, 2))
  }, error = function(e) {
    warning("Failed to create PC contribution plot: ", e$message)
    return(NULL)
  })
}

# Print method for shape_analysis_result ----

#' Print method for shape_analysis_result objects
#' @param x A shape_analysis_result object
#' @param ... Additional arguments (ignored)
#' @export
print.shape_analysis_result <- function(x, ...) {
  cat("Shape Analysis Results\n")
  cat("======================\n\n")
  cat("Analysis metadata:\n")
  cat("  Number of shapes:", x$metadata$n_shapes, "\n")
  cat("  Harmonics:", x$metadata$n_harmonics, "\n")
  cat("  Normalization:", x$metadata$normalization, "\n")
  cat("  Start point:", x$metadata$start_point, "\n")
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n\n")
  
  cat("Output files:\n")
  cat("  PCA scores:", x$output_path, "\n")
  cat("  Summary:", x$summary_txt_path, "\n")
  cat("  PC plot:", x$pc_plot_jpg_path, "\n")
  cat("  Reconstruction model:", x$reconstruction_model_path, "\n")
  cat("  Reconstruction info:", x$reconstruction_info_path, "\n\n")
  
  cat("PCA Summary:\n")
  cat(x$summary, "\n")
}

# Reconstruction Model Loader ----

#' Load Shape Reconstruction Model
#'
#' Loads a reconstruction model saved by `shape_analysis()` that contains all the
#' data needed to reconstruct shapes from PC scores: eigenvectors, EFA coefficients,
#' and analysis parameters.
#'
#' @param model_path Character string specifying the path to the reconstruction model
#'   RDS file (typically named `*_reconstruction_model.rds`).
#' @param validate Logical indicating whether to validate the model structure. Default: TRUE.
#' @param verbose Logical indicating whether to print loading messages. Default: TRUE.
#'
#' @return A list containing:
#'   \describe{
#'     \item{rotation}{Matrix of eigenvectors (loadings) from PCA}
#'     \item{center}{Centering vector from PCA}
#'     \item{sdev}{Standard deviations from PCA}
#'     \item{efa_coe}{EFA coefficient matrix from Momocs}
#'     \item{efa_method}{EFA method attribute}
#'     \item{efa_norm}{EFA normalization parameters}
#'     \item{parameters}{List of analysis parameters (norm, harmonics, start_point, etc.)}
#'     \item{variance_explained}{Vector of variance explained by each PC}
#'     \item{metadata}{Metadata about model creation (version, date, etc.)}
#'   }
#'
#' @details
#' This function loads reconstruction models created by `shape_analysis()`. The model
#' contains all necessary information to reconstruct shapes from principal component scores:
#' 
#' - **PCA components**: Eigenvectors (rotation matrix), centering values, and standard deviations
#' - **EFA coefficients**: Fourier coefficient matrix and normalization parameters
#' - **Parameters**: Analysis settings used (normalization, harmonics, start point)
#' - **Metadata**: Version info and creation timestamp
#'
#' The loaded model can be used with shape reconstruction functions to generate
#' contours from PC scores.
#'
#' @examples
#' \dontrun{
#' # Load a reconstruction model
#' model <- load_reconstruction_model("shape_analysis_reconstruction_model.rds")
#'
#' # Check model information
#' print(model$parameters)
#' print(model$metadata)
#'
#' # Use with reconstruction function (when implemented)
#' # reconstructed <- reconstruct_shape_from_pca(model, pc_scores = c(PC1 = 2.0, PC2 = -1.5))
#' }
#'
#' @export
load_reconstruction_model <- function(model_path, validate = TRUE, verbose = TRUE) {
  
  # Input validation ----
  if (!is.character(model_path) || length(model_path) != 1 || !nzchar(model_path)) {
    stop("'model_path' must be a non-empty character string", call. = FALSE)
  }
  
  if (!file.exists(model_path)) {
    stop("Reconstruction model file does not exist: ", model_path, call. = FALSE)
  }
  
  if (!is.logical(validate) || length(validate) != 1) {
    stop("'validate' must be a single logical value", call. = FALSE)
  }
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
  
  # Load model ----
  if (verbose) message("Loading reconstruction model from: ", model_path)
  
  model <- tryCatch({
    readRDS(model_path)
  }, error = function(e) {
    stop("Failed to load reconstruction model: ", e$message, call. = FALSE)
  })
  
  # Validate model structure ----
  if (validate) {
    if (verbose) message("Validating model structure...")
    
    required_components <- c("rotation", "center", "sdev", "parameters", "metadata")
    missing_components <- setdiff(required_components, names(model))
    
    if (length(missing_components) > 0) {
      stop("Reconstruction model is missing required components: ",
           paste(missing_components, collapse = ", "), call. = FALSE)
    }
    
    # Check for either efa_object (new format) or efa_coe (old format)
    if (is.null(model$efa_object) && is.null(model$efa_coe)) {
      stop("Reconstruction model is missing EFA data (neither efa_object nor efa_coe found)", call. = FALSE)
    }
    
    # Check metadata version
    if (!is.null(model$metadata$format_version)) {
      if (verbose) {
        message("Model format version: ", model$metadata$format_version)
        message("Created: ", format(model$metadata$created_date))
      }
      
      # Future version compatibility checks can go here
      supported_versions <- c("1.0")
      if (!model$metadata$format_version %in% supported_versions) {
        warning("Reconstruction model format version ", model$metadata$format_version,
                " may not be fully supported. Supported versions: ",
                paste(supported_versions, collapse = ", "))
      }
    }
    
    # Check parameter structure
    required_params <- c("norm", "harmonics", "start_point", "n_components", "n_specimens")
    missing_params <- setdiff(required_params, names(model$parameters))
    
    if (length(missing_params) > 0) {
      warning("Model parameters missing: ", paste(missing_params, collapse = ", "))
    }
    
    # Check dimensions consistency
    if (!is.null(model$rotation) && !is.null(model$center)) {
      if (nrow(model$rotation) != length(model$center)) {
        warning("Dimension mismatch: rotation matrix rows (", nrow(model$rotation),
                ") != center vector length (", length(model$center), ")")
      }
    }
    
    if (verbose) {
      message("Model validation passed")
      message("  Specimens: ", model$parameters$n_specimens)
      message("  PCs: ", model$parameters$n_components)
      message("  Harmonics: ", model$parameters$harmonics)
      message("  Normalization: ", model$parameters$norm)
    }
  }
  
  # Return model
  if (verbose) message("Reconstruction model loaded successfully")
  
  return(model)
}