#' Analyze Shape Images and Perform PCA
#'
#' This function processes a directory of shape images, normalizes the shapes,
#' performs Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA),
#' displays a plot of PC contributions, and saves PCA scores to an Excel file.
#'
#' @param shape_dir A character string specifying the directory containing shape files
#'   (images in JPG format). The directory must exist and contain at least one valid JPG file.
#' @param norm Logical. Should the Fourier descriptors be normalized? If TRUE, shapes are
#'   normalized to the first harmonic. If FALSE, shapes are normalized to the longest radius.
#'   Default is TRUE.
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
#'     \item{scores}{Data frame of PCA scores}
#'     \item{summary}{Character string of PCA summary}
#'     \item{output_path}{Path where Excel results were saved}
#'     \item{summary_txt_path}{Path to saved PCA summary .txt file}
#'     \item{pc_plot_jpg_path}{Path to saved PC contributions .jpg}
#'     \item{metadata}{List of analysis metadata (n_shapes, harmonics, etc.)}
#'   }
#'
#' @details
#' The function uses the `Momocs` package to import, normalize, and analyze shape outlines:
#' - **Import**: Reads JPG/JPEG files and converts to shape outlines
#' - **Normalization**: Centers, scales, and aligns shapes to consistent orientation
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
                          use_raw_shapes = FALSE,
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
  
  # Normalize shapes (unless raw mode) ----
  if (isTRUE(use_raw_shapes)) {
    if (verbose) message("Using shapes as-is (no centering/scaling/orientation changes)")
    normalized_shapes <- shapes
  } else {
    if (verbose) message("Normalizing shapes...")
    normalized_shapes <- .normalize_shapes(shapes, start_point, align_orientation, verbose)
  }
  
  # Perform Elliptical Fourier Analysis ----
  if (verbose) message("Performing Elliptical Fourier Analysis...")
  # If using raw shapes, do not apply EFA normalization (keep as-is)
  effective_norm <- if (isTRUE(use_raw_shapes)) FALSE else norm
  efa_results <- .perform_efa(normalized_shapes, effective_norm, harmonics, verbose)
  
  # Perform PCA ----
  if (verbose) message("Performing Principal Component Analysis...")
  pca_results <- .perform_pca(efa_results, verbose)
  
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
  
  # Prepare metadata
  metadata <- list(
    n_shapes = length(shapes),
    n_harmonics = ifelse(is.null(harmonics), "automatic", harmonics),
    normalization = norm,
    used_raw_shapes = isTRUE(use_raw_shapes),
    start_point = start_point,
    analysis_date = Sys.time()
  )
  
  if (verbose) message("Analysis completed successfully!")
  
  # Return comprehensive results ----
  structure(
    list(
      pca_results = pca_results,
      scores = scores,
      summary = analysis_summary,
      output_path = output_file_path,
      summary_txt_path = summary_txt_path,
      pc_plot_jpg_path = pc_plot_jpg_path,
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
.perform_efa <- function(normalized_shapes, norm, harmonics, verbose) {
  tryCatch({
    if (is.null(harmonics)) {
      efa_results <- Momocs::efourier(normalized_shapes, norm = norm)
    } else {
      efa_results <- Momocs::efourier(normalized_shapes, norm = norm, nb.h = harmonics)
    }
    
    if (verbose) message("EFA completed with normalization: ", norm)
    return(efa_results)
  }, error = function(e) {
    stop("Elliptical Fourier Analysis failed: ", e$message, call. = FALSE)
  })
}

#' Perform Principal Component Analysis
#' @noRd
.perform_pca <- function(efa_results, verbose) {
  # First attempt with scaling; if zero-variance columns cause an error, retry without scaling
  tryCatch({
    pca_results <- Momocs::PCA(efa_results, center = TRUE, scale. = TRUE)
    if (verbose) message("PCA completed successfully")
    return(pca_results)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("zero column|constant/zero", msg, ignore.case = TRUE)) {
      if (verbose) message("Zero-variance descriptors detected; retrying PCA without scaling...")
      pca_results2 <- Momocs::PCA(efa_results, center = TRUE, scale. = FALSE)
      if (verbose) message("PCA completed successfully (no scaling)")
      return(pca_results2)
    }
    stop("Principal Component Analysis failed: ", msg, call. = FALSE)
  })
}

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
  cat("  Analysis date:", format(x$metadata$analysis_date), "\n")
  cat("  Output saved to:", x$output_path, "\n\n")
  cat("PCA Summary:\n")
  cat(x$summary, "\n")
}