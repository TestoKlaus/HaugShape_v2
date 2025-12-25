# Gap Detection Quick Start Guide

## Overview
The Gap Detection feature identifies statistically rigorous gaps (regions of constraint) in your morphospace, accounting for both measurement and sampling uncertainty.

## Workflow

### Step 1: Run Gap Detection Analysis

1. **Launch the app**
   ```r
   library(HaugShapeV2)
   run_haug_app()
   ```

2. **Navigate to "Gap Detection" tab** (menu item 5.5)

3. **Load your PCA data**
   - Click "Browse" in the "Load PCA Data" box
   - Select your PCA scores file (CSV, Excel, or RDS)
   - File must contain columns: `PC1`, `PC2`, `PC3`, etc.
   - Preview will show your data

4. **Configure parameters** (or use defaults)
   - **Measurement Uncertainty**: 5% (recommended)
   - **Grid Resolution**: 150 (balance of speed vs precision)
     - Lower (50-100): Faster, less precise
     - Higher (200-300): Slower, more precise
   - **Monte Carlo Iterations**: 100 (measurement uncertainty)
     - Minimum: 50 for quick tests
     - Recommended: 100-200 for publication
   - **Bootstrap Iterations**: 200 (sampling uncertainty)
     - Minimum: 100 for quick tests
     - Recommended: 200-500 for publication
   - **Maximum PC**: 4 (analyze PC1-PC4)
   - **Hull Type**: Alpha Hull (concave, preferred)
   - **Certainty Thresholds**: 0.80, 0.90, 0.95

5. **Set output location**
   - ✅ "Automatically save results to file" should be checked
   - Specify output folder (default: current working directory)
   - Click "Browse..." to select a different folder
   - Results filename: `gap_results_YYYYMMDD_HHMMSS.rds`

6. **Run analysis**
   - Click "Detect Gaps" button
   - Progress indicator will show (may take 2-5 minutes)
   - Results will appear in visualization panel
   - **File automatically saved** to your specified folder

7. **Review results**
   - View gap visualization (select PC pair and threshold)
   - Check gap metrics table
   - Note the saved file path displayed in green box
   - Download summary CSV if needed

### Step 2: Overlay Gaps on Morphospace Plot

1. **Navigate to "Plotting" tab** (menu item 5)

2. **Load your morphospace data**
   - Import data in "Data Import" tab first
   - Or use data already loaded

3. **Configure basic plot**
   - Select X column (e.g., PC1)
   - Select Y column (e.g., PC2)
   - Configure styling, groups, hulls as desired

4. **Enable gap overlay**
   - Scroll to "Features - Gap Overlay" box
   - ✅ Check "Show morphospace gaps"
   - Click "Browse" and select your saved gap results file
     - File is in the folder you specified in Step 1
     - Look for `gap_results_YYYYMMDD_HHMMSS.rds`
   
5. **Configure gap display**
   - **Gap PC Pair**: Auto-selects to match your X/Y axes
   - **Certainty Threshold**: Choose threshold (0.80, 0.90, or 0.95)
   - **Display Mode**:
     - Heatmap: Color gradient showing gap certainty
     - Polygons: Black outlines of gap regions
     - Both: Combined visualization (recommended)
   - **Gap Overlay Alpha**: Transparency (0.5 = 50%)
   - **Colors**: Customize gradient (white → yellow → red)

6. **Render plot**
   - Click "Render plot" button
   - Your morphospace will display with gap overlay
   - Export plot as RDS for further editing in RStudio

## Tips & Recommendations

### For Initial Exploration
```r
Uncertainty: 5%
Grid: 100
Monte Carlo: 50
Bootstrap: 100
Max PCs: 2
```
**Runtime**: ~1-2 minutes

### For Publication-Quality Results
```r
Uncertainty: 5%
Grid: 200
Monte Carlo: 200
Bootstrap: 500
Max PCs: 4
Hull: Alpha
```
**Runtime**: ~5-10 minutes (enable parallel processing)

### Understanding Results

**Gap Certainty** = Gap Probability × Gap Stability

- **Gap Probability**: Proportion of Monte Carlo iterations where cell was unoccupied
  - High value = consistently unoccupied despite measurement uncertainty
  
- **Gap Stability**: Proportion of bootstrap iterations where cell was classified as gap
  - High value = gap persists across different data samples

- **Combined Certainty**: Both must be high for confident gap classification
  - 0.80+ = Potential gap
  - 0.90+ = High-confidence gap
  - 0.95+ = Very high-confidence gap

### Interpreting Gap Metrics

In the summary table:

- **area**: Size of gap region in PC space
- **mean_certainty**: Average certainty within gap
- **max_certainty**: Maximum certainty within gap
- **gap_depth**: Distance to nearest occupied point
  - Higher depth = more isolated gap
- **centroid_x, centroid_y**: Center of gap region

### Common Issues

**Problem**: No gaps detected
- **Solution**: Lower certainty thresholds, increase grid resolution, or check if data truly has gaps

**Problem**: Too many small gaps
- **Solution**: Increase certainty thresholds, reduce grid resolution, or use higher bootstrap iterations

**Problem**: Analysis is slow
- **Solution**: Reduce grid resolution, fewer iterations, or enable parallel processing

**Problem**: Can't find saved file
- **Solution**: Check the green box showing saved file path, or use download buttons as backup

## File Locations

### Automatic Save Location
Default: Your current working directory
```r
getwd()  # Check current directory
setwd("C:/your/preferred/path")  # Change if needed
```

### Finding Your Results
After analysis, look for the saved file path in the **green box** that appears after "Analysis Complete!"

Example path:
```
C:/Projects/MyStudy/gap_results_20251226_143022.rds
```

### Loading Results in Plotting Module
1. Click "Browse" in Gap Overlay section
2. Navigate to your output folder
3. Select `gap_results_YYYYMMDD_HHMMSS.rds`
4. Status will show "✓ Gap results loaded"

## Programmatic Usage (Outside Shiny)

```r
library(HaugShapeV2)

# Load your PCA scores
pca_data <- read.csv("your_pca_scores.csv")

# Run gap detection
gaps <- detect_morphospace_gaps(
  pca_scores = pca_data,
  uncertainty = 0.05,
  grid_resolution = 150,
  monte_carlo_iterations = 100,
  bootstrap_iterations = 200,
  max_pcs = 4
)

# Save results
saveRDS(gaps, "gap_results.rds")

# View summary
print(gaps)
gaps$summary_table

# Access specific PC pair
pc1_pc2 <- gaps$results$`PC1-PC2`

# Plot gap certainty
image(pc1_pc2$gap_certainty, 
      main = "Gap Certainty: PC1-PC2",
      xlab = "PC1", ylab = "PC2")
```

## Next Steps

After identifying high-certainty gaps:

1. **Biological Interpretation**: Why are these regions unoccupied?
   - Functional constraints?
   - Developmental limitations?
   - Ecological restrictions?

2. **Shape Reconstruction** (future feature): 
   - Select gap locations
   - Inverse transform: PC → EFA coefficients → outline
   - Visualize "forbidden shapes"

3. **Comparative Analysis**:
   - Compare gaps across different groups
   - Temporal gap evolution
   - Phylogenetic gap patterns

## Support

For issues or questions:
- Check [GAP_DETECTION_IMPLEMENTATION.md](GAP_DETECTION_IMPLEMENTATION.md) for technical details
- See [README.md](README.md) for general package documentation
- Open an issue on GitHub: https://github.com/TestoKlaus/HaugShape_v2/issues
