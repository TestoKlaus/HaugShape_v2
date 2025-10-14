# HaugShape v2 Shiny Application

This directory contains the comprehensive Shiny web application for HaugShape v2, providing an intuitive interface for geometric morphometric analysis.

## Application Structure

```
inst/shiny/
├── app.R           # Main application file (UI + Server)
├── global.R        # Global variables, constants, and helper functions
├── server_utils.R  # Server-side utility functions
└── www/           # Web assets (CSS, JavaScript, images)
    ├── custom.css
    ├── app.js
    └── images/
```

## Features

### Core Analysis Modules
- **Shape Analysis**: Elliptical Fourier Analysis (EFA) and Principal Component Analysis (PCA)
- **Morphometric Overview**: Multi-panel visualization with customizable layouts
- **Shape Plotting**: Interactive scatter plots, biplots, and shape reconstructions
- **Clustering Analysis**: Multiple algorithms with validation metrics

### Image Processing Tools
- **Format Conversion**: Batch convert between PNG, JPEG, TIFF formats
- **Image Cropping**: Flexible cropping with multiple methods
- **Image Splitting**: Split images using various methods and ratios
- **Shape Completion**: Complete partial shapes using symmetry detection

### Advanced Analysis
- **Hull Analysis**: Convex, alpha, and concave hull computations
- **Shape Mapping**: Map analysis results to external datasets
- **Elbow Analysis**: Determine optimal cluster numbers
- **Shape Morphing**: Advanced morphing algorithms with distance transforms

### Specialized Visualization
- **Haug Panels**: Advanced morphometric panel layouts
- **Momocs Integration**: Specialized plots with shape reconstructions

### Utility Features
- **Batch Processing**: Handle large datasets efficiently
- **Session Management**: Save and restore analysis sessions
- **Export & Reports**: Generate comprehensive reports in multiple formats
- **Interactive Help**: Built-in documentation and tutorials

## Running the Application

### Method 1: Using the R Function
```r
# Install and load the HaugShape package
library(HaugShape)

# Launch the Shiny app
run_haug_app()

# Or with custom settings
run_haug_app(port = 3838, launch.browser = TRUE)
```

### Method 2: Direct Shiny Execution
```r
# Run from the package directory
shiny::runApp("inst/shiny")

# Or from installed package
app_dir <- system.file("shiny", package = "HaugShape")
shiny::runApp(app_dir)
```

### Method 3: Development Mode
```r
# For development with live reload
options(shiny.autoreload = TRUE)
shiny::runApp("inst/shiny", display.mode = "showcase")
```

## Dependencies

The Shiny application requires the following R packages:

### Core Shiny Packages
- `shiny` (≥ 1.7.0)
- `shinydashboard` (≥ 0.7.0)
- `DT` (≥ 0.18)
- `plotly` (≥ 4.9.0)

### UI Enhancement Packages
- `shinyWidgets` (≥ 0.6.0)
- `shinycssloaders` (≥ 1.0.0)
- `colourpicker` (≥ 1.1.0)
- `shinyFiles` (≥ 0.9.0)

### Data Processing Packages
- `ggplot2` (≥ 3.3.0)
- `dplyr` (≥ 1.0.0)
- `readxl` (≥ 1.3.0)
- `openxlsx` (≥ 4.2.0)

### Analysis Packages
- `Momocs` (≥ 1.3.0)
- `magick` (≥ 2.7.0)
- `imager` (≥ 0.42.0)

Install missing dependencies:
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly", "shinyWidgets",
  "shinycssloaders", "colourpicker", "shinyFiles", "ggplot2",
  "dplyr", "readxl", "openxlsx"
))
```

## Application Workflow

### 1. Data Upload
- Upload shape image files (PNG, JPEG, TIFF)
- Upload morphometric data files (CSV, Excel)
- Configure processing options
- Validate and preview uploaded data

### 2. Shape Analysis
- Select analysis method (EFA, PCA, or both)
- Configure analysis parameters
- Run batch processing for large datasets
- Review interactive results

### 3. Visualization
- Create customizable plots
- Add statistical overlays
- Export publication-quality figures
- Generate comprehensive overviews

### 4. Advanced Analysis
- Perform clustering analysis
- Compute convex hulls
- Map data to external sources
- Morph between shapes

### 5. Export & Reports
- Download analysis results
- Generate PDF/HTML reports
- Export individual plots
- Save session data

## Configuration

### Custom Themes
Add custom CSS themes in `www/custom.css`:
```css
.custom-theme {
  /* Your custom styles */
}
```

### Advanced Settings
Modify global settings in `global.R`:
```r
# Custom color palettes
CUSTOM_PALETTE <- c("#your", "#colors", "#here")

# Analysis parameters
DEFAULT_HARMONICS <- 15
DEFAULT_COMPONENTS <- 12
```

## Troubleshooting

### Common Issues

**1. App won't start**
- Check that all required packages are installed
- Verify R version compatibility (≥ 4.0.0 recommended)
- Check for port conflicts

**2. File upload errors**
- Verify file formats are supported
- Check file size limits
- Ensure proper file permissions

**3. Analysis failures**
- Validate input data format
- Check for missing values
- Reduce dataset size for testing

**4. Plot rendering issues**
- Update web browser
- Clear browser cache
- Check JavaScript console for errors

### Performance Optimization

For large datasets:
- Enable batch processing mode
- Reduce number of components/harmonics
- Use data sampling for preview
- Close other browser tabs

### Memory Management

Monitor memory usage:
```r
# Check memory usage
gc()
pryr::mem_used()

# Clear large objects
rm(large_object)
gc()
```

## Development

### Adding New Features

1. **New Analysis Module**
   - Add UI elements in `app.R`
   - Implement server logic
   - Add helper functions in `server_utils.R`
   - Update documentation

2. **Custom Visualizations**
   - Create plot functions
   - Add UI controls
   - Implement interactivity with plotly
   - Add export functionality

3. **Data Processing Tools**
   - Add processing functions
   - Implement validation
   - Add progress indicators
   - Handle batch processing

### Testing

Test the application:
```r
# Load testing tools
library(shinytest)

# Record and run tests
app <- ShinyDriver$new("inst/shiny")
app$setInputs(...)  # Interact with app
app$getValues()     # Check outputs
```

## Deployment

### Local Network Deployment
```r
# Run on specific host/port
run_haug_app(host = "0.0.0.0", port = 3838)
```

### Shiny Server Deployment
1. Copy app files to Shiny Server directory
2. Install required packages on server
3. Configure server settings
4. Monitor logs for issues

### Cloud Deployment (shinyapps.io)
```r
library(rsconnect)

# Deploy to shinyapps.io
rsconnect::deployApp("inst/shiny", appName = "haugshape-v2")
```

## Support

For technical support:
- Check the built-in help documentation
- Review function examples and tutorials
- Submit issues on GitHub repository
- Contact the development team

## License

This Shiny application is part of the HaugShape v2 package and is distributed under the MIT License. See the main package documentation for full license terms.