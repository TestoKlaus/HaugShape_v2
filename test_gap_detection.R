# Test Script for Gap Detection Feature
# This script demonstrates the gap detection functionality

# Load package (assuming it's installed or using devtools::load_all())
# library(HaugShapeV2)
# Or if testing in development:
# devtools::load_all()

# ============================================================================
# Test 1: Basic Gap Detection with Simulated Data
# ============================================================================

test_basic_gap_detection <- function() {
  cat("\n=== Test 1: Basic Gap Detection ===\n")
  
  # Create simulated PCA data with a gap
  set.seed(123)
  
  # Two clusters with a gap between them
  cluster1 <- data.frame(
    PC1 = rnorm(50, mean = -2, sd = 0.5),
    PC2 = rnorm(50, mean = 0, sd = 0.5),
    PC3 = rnorm(50, mean = 0, sd = 0.3)
  )
  
  cluster2 <- data.frame(
    PC1 = rnorm(50, mean = 2, sd = 0.5),
    PC2 = rnorm(50, mean = 0, sd = 0.5),
    PC3 = rnorm(50, mean = 0, sd = 0.3)
  )
  
  pca_scores <- rbind(cluster1, cluster2)
  rownames(pca_scores) <- paste0("specimen_", 1:100)
  
  cat("Created simulated data: 100 specimens, 3 PCs, with intentional gap\n")
  
  # Run gap detection with reduced iterations for quick testing
  cat("Running gap detection (this may take 1-2 minutes)...\n")
  
  gaps <- detect_morphospace_gaps(
    pca_scores = pca_scores,
    uncertainty = 0.05,
    grid_resolution = 100,         # Reduced for speed
    monte_carlo_iterations = 50,   # Reduced for speed
    bootstrap_iterations = 50,     # Reduced for speed
    certainty_thresholds = c(0.80, 0.90),
    max_pcs = 2,                   # Only PC1-PC2
    hull_type = "convex",          # Faster than alpha
    verbose = TRUE
  )
  
  # Print results
  cat("\n")
  print(gaps)
  
  # Check results
  if (nrow(gaps$summary_table) > 0) {
    cat("\n✓ SUCCESS: Gap detected!\n")
    cat("\nGap details:\n")
    print(gaps$summary_table[, c("pc_pair", "threshold", "area", "mean_certainty", "gap_depth")])
  } else {
    cat("\n✗ WARNING: No gaps detected (this might happen with small iterations)\n")
  }
  
  return(gaps)
}

# ============================================================================
# Test 2: Export and Import Gap Results
# ============================================================================

test_export_import <- function(gaps) {
  cat("\n=== Test 2: Export and Import ===\n")
  
  # Save to RDS
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(gaps, temp_file)
  cat("Saved gap results to:", temp_file, "\n")
  
  # Load back
  loaded_gaps <- readRDS(temp_file)
  cat("Loaded gap results from file\n")
  
  # Verify
  if (inherits(loaded_gaps, "morphospace_gaps")) {
    cat("✓ SUCCESS: Gap results loaded correctly\n")
  } else {
    cat("✗ FAILED: Gap results not loaded correctly\n")
  }
  
  # Clean up
  unlink(temp_file)
  
  return(loaded_gaps)
}

# ============================================================================
# Test 3: Visualization (if plotting available)
# ============================================================================

test_visualization <- function(gaps) {
  cat("\n=== Test 3: Visualization ===\n")
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("⚠ Skipping: ggplot2 not available\n")
    return(NULL)
  }
  
  # Get results for PC1-PC2
  pc1_pc2 <- gaps$results$`PC1-PC2`
  
  if (is.null(pc1_pc2)) {
    cat("⚠ No PC1-PC2 results available\n")
    return(NULL)
  }
  
  # Create simple visualization
  gap_df <- expand.grid(
    x = pc1_pc2$grid_x,
    y = pc1_pc2$grid_y
  )
  gap_df$certainty <- as.vector(pc1_pc2$gap_certainty)
  gap_df <- gap_df[!is.na(gap_df$certainty), ]
  
  p <- ggplot2::ggplot(gap_df, ggplot2::aes(x = x, y = y, fill = certainty)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient2(
      low = "white",
      mid = "yellow", 
      high = "red",
      midpoint = 0.5,
      name = "Gap\nCertainty"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Gap Detection: PC1-PC2",
      x = "PC1",
      y = "PC2"
    )
  
  cat("✓ Created gap visualization plot\n")
  cat("  Use print(p) to display the plot\n")
  
  return(p)
}

# ============================================================================
# Run All Tests
# ============================================================================

run_all_tests <- function() {
  cat("\n")
  cat("========================================\n")
  cat("  Gap Detection Feature Test Suite\n")
  cat("========================================\n")
  
  # Test 1
  gaps <- test_basic_gap_detection()
  
  # Test 2
  if (!is.null(gaps)) {
    loaded_gaps <- test_export_import(gaps)
  }
  
  # Test 3
  if (!is.null(gaps)) {
    plot_obj <- test_visualization(gaps)
  }
  
  cat("\n")
  cat("========================================\n")
  cat("  All Tests Complete!\n")
  cat("========================================\n")
  cat("\nNext steps:\n")
  cat("1. Run the Shiny app: shiny::runApp('app.R')\n")
  cat("2. Navigate to 'Gap Detection' tab\n")
  cat("3. Load your PCA scores\n")
  cat("4. Configure parameters and click 'Detect Gaps'\n")
  cat("5. View results and export\n")
  cat("6. Go to 'Plotting' tab to overlay gaps on morphospace\n\n")
  
  invisible(list(gaps = gaps, plot = plot_obj))
}

# ============================================================================
# Execute Tests (uncomment to run)
# ============================================================================

# Uncomment the line below to run all tests:
# results <- run_all_tests()

# Or run individual tests:
# gaps <- test_basic_gap_detection()
# loaded <- test_export_import(gaps)
# plot_obj <- test_visualization(gaps)
# print(plot_obj)  # Display the plot

cat("\nTest script loaded. Run run_all_tests() to execute all tests.\n")
