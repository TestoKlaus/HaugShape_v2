HaugShape: Morphometric Shape Analysis and Visualization Tools
========================================================

## Overview

HaugShape is a comprehensive R package for morphometric shape analysis, providing tools for:

- **Shape Analysis**: Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA)
- **Visualization**: Advanced plotting functions for hull plots, contour plots, and boxplots  
- **Data Processing**: Shape preprocessing, normalization, and statistical analysis
- **Interactive Tools**: Shiny application for interactive shape analysis

## Installation

You can install HaugShape from GitHub using:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install HaugShape
devtools::install_github("TestoKlaus/HaugShape_v2")
```

## Quick Start

```r
library(HaugShape)

# Run the interactive Shiny app (development)
# Option 1: From the project root
# source('app.R')

# Option 2: Using the helper function
source('R/run_haug_app.R')
run_haug_app()

# Analyze shapes in a directory
result <- shape_analysis(
  shape_dir = "path/to/your/shapes",
  output_file = "analysis_results.xlsx"
)

# Create overview plots
overview <- Haug_overview(
  data = your_data,
  cols = c("PC1", "PC2", "PC3", "PC4"),
  group_col = "species",
  export_pdf = TRUE
)
```

## Main Functions

- `shape_analysis()`: Perform comprehensive shape analysis with EFA and PCA
- `Haug_overview()`: Generate multi-panel overview plots
- `run_haug_app()`: Launch interactive Shiny application
- `shape_plot()`: Create customizable shape plots
- `cluster_plot()`: Generate cluster analysis plots

## Requirements

- R >= 4.0.0
- Required packages: Momocs, ggplot2, dplyr, openxlsx, shiny

## License

MIT License

## Contact

For questions and bug reports, please visit: https://github.com/TestoKlaus/HaugShape_v2/issues