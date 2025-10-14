# HaugShape v2 - Comprehensive Morphometric Analysis Shiny Application
# A full-featured web interface for geometric morphometric analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinyFiles)
library(shinycssloaders)
library(colourpicker)

# Source HaugShape functions (they should be available if package is loaded)
# library(HaugShape)

# Define UI
ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(
    title = "HaugShape v2 - Morphometric Analysis Suite",
    titleWidth = 400
  ),
  
  # Sidebar ----
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar_menu",
      
      # Data Management
      menuItem("Data Upload", tabName = "upload", icon = icon("upload"),
               badgeLabel = "Start", badgeColor = "green"),
      
      # Core Analysis Modules
      menuItem("Shape Analysis", tabName = "shape_analysis", icon = icon("shapes")),
      menuItem("Morphometric Overview", tabName = "overview", icon = icon("chart-area")),
      menuItem("Shape Plotting", tabName = "shape_plot", icon = icon("scatter-chart")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      
      # Image Processing
      menuItem("Image Processing", tabName = "image_proc", icon = icon("image"),
        menuSubItem("Format Conversion", tabName = "convert"),
        menuSubItem("Image Cropping", tabName = "cropping"),
        menuSubItem("Image Splitting", tabName = "splitting"),
        menuSubItem("Shape Completion", tabName = "completion")
      ),
      
      # Advanced Analysis
      menuItem("Advanced Tools", tabName = "advanced", icon = icon("cogs"),
        menuSubItem("Hull Analysis", tabName = "hull_analysis"),
        menuSubItem("Shape Mapping", tabName = "shape_mapping"),
        menuSubItem("Elbow Analysis", tabName = "elbow"),
        menuSubItem("Shape Morphing", tabName = "morphing")
      ),
      
      # Specialized Visualization
      menuItem("Specialized Plots", tabName = "specialized", icon = icon("chart-line"),
        menuSubItem("Haug Panels", tabName = "haug_panels"),
        menuSubItem("Momocs Plots", tabName = "momocs_plots")
      ),
      
      # Utilities and Help
      menuItem("Batch Processing", tabName = "batch", icon = icon("tasks")),
      menuItem("Export & Reports", tabName = "export", icon = icon("file-export")),
      menuItem("Documentation", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # Body ----
  dashboardBody(
    
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .main-header .logo { font-weight: bold; }
        .box-header { background: linear-gradient(45deg, #3c8dbc, #367fa9); }
        .box-title { color: white !important; font-weight: bold; }
        .progress-bar { background-color: #3c8dbc; }
        .btn-success { background-color: #00a65a; }
        .btn-warning { background-color: #f39c12; }
        .sidebar-menu > li.active > a { background-color: #367fa9; }
        .nav-tabs-custom > .nav-tabs > li.active > a { background-color: #3c8dbc; color: white; }
      "))
    ),
    
    # Tab Items ----
    tabItems(
      
      # Data Upload Tab ----
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Data Upload & Management", status = "primary", solidHeader = TRUE,
            width = 12, height = "auto",
            
            tabsetPanel(
              # Shape Files Upload
              tabPanel("Shape Files",
                br(),
                fluidRow(
                  column(6,
                    fileInput("shape_files", "Upload Shape Images",
                             multiple = TRUE,
                             accept = c(".jpg", ".jpeg", ".png", ".tiff", ".bmp")),
                    
                    conditionalPanel("output.shape_files_uploaded",
                      h4("File Processing Options"),
                      checkboxInput("auto_convert", "Auto-convert to consistent format", TRUE),
                      selectInput("target_format", "Target Format:",
                                 choices = c("PNG" = "png", "JPEG" = "jpg", "TIFF" = "tiff")),
                      
                      h4("Shape Validation"),
                      checkboxInput("validate_binary", "Validate binary shapes", TRUE),
                      numericInput("threshold", "Binary Threshold:", 0.5, min = 0, max = 1, step = 0.1),
                      
                      actionButton("process_shapes", "Process Shape Files", 
                                  class = "btn-success", icon = icon("cog"))
                    )
                  ),
                  column(6,
                    conditionalPanel("output.shape_files_uploaded",
                      h4("Uploaded Files Preview"),
                      withSpinner(DT::dataTableOutput("shape_files_table"))
                    )
                  )
                )
              ),
              
              # Data Files Upload  
              tabPanel("Data Files",
                br(),
                fluidRow(
                  column(6,
                    fileInput("data_file", "Upload Morphometric Data",
                             accept = c(".csv", ".xlsx", ".xls", ".txt")),
                    
                    conditionalPanel("output.data_file_uploaded",
                      h4("Data Processing Options"),
                      checkboxInput("has_header", "File has header", TRUE),
                      selectInput("separator", "Separator:",
                                 choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
                      
                      h4("Column Selection"),
                      selectInput("id_column", "ID Column:", choices = NULL),
                      selectInput("group_column", "Group Column:", choices = NULL),
                      selectInput("analysis_columns", "Analysis Columns:", 
                                 choices = NULL, multiple = TRUE),
                      
                      actionButton("process_data", "Process Data File", 
                                  class = "btn-success", icon = icon("table"))
                    )
                  ),
                  column(6,
                    conditionalPanel("output.data_file_uploaded",
                      h4("Data Preview"),
                      withSpinner(DT::dataTableOutput("data_preview"))
                    )
                  )
                )
              ),
              
              # Session Management
              tabPanel("Session",
                br(),
                h4("Session Management"),
                fluidRow(
                  column(6,
                    h5("Save Current Session"),
                    textInput("session_name", "Session Name:", value = paste0("session_", Sys.Date())),
                    actionButton("save_session", "Save Session", class = "btn-info", icon = icon("save")),
                    
                    br(), br(),
                    h5("Load Previous Session"),
                    fileInput("load_session", "Load Session File", accept = ".rds"),
                    actionButton("restore_session", "Restore Session", class = "btn-warning", icon = icon("upload"))
                  ),
                  column(6,
                    h5("Session Status"),
                    verbatimTextOutput("session_status"),
                    
                    br(),
                    h5("Clear All Data"),
                    actionButton("clear_all", "Clear All Data", class = "btn-danger", 
                               icon = icon("trash"), onclick = "return confirm('Are you sure?');")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Shape Analysis Tab ----
      tabItem(tabName = "shape_analysis",
        fluidRow(
          # Analysis Settings
          box(
            title = "Shape Analysis Configuration", status = "primary", solidHeader = TRUE,
            width = 4,
            
            h4("Analysis Method"),
            radioButtons("analysis_method", NULL,
              choices = c("Elliptical Fourier Analysis" = "efa",
                         "Principal Component Analysis" = "pca",
                         "Both EFA and PCA" = "both")),
            
            conditionalPanel("input.analysis_method == 'efa' || input.analysis_method == 'both'",
              h5("EFA Settings"),
              numericInput("efa_harmonics", "Number of Harmonics:", 10, min = 3, max = 50),
              checkboxInput("efa_normalize", "Normalize EFA coefficients", TRUE)
            ),
            
            conditionalPanel("input.analysis_method == 'pca' || input.analysis_method == 'both'",
              h5("PCA Settings"),
              numericInput("pca_components", "Number of Components:", 10, min = 2, max = 50),
              checkboxInput("pca_scale", "Scale variables", TRUE),
              checkboxInput("pca_center", "Center variables", TRUE)
            ),
            
            h4("Processing Options"),
            checkboxInput("batch_process", "Enable batch processing", FALSE),
            numericInput("batch_size", "Batch size:", 50, min = 10, max = 500),
            
            h4("Statistical Analysis"),
            checkboxInput("compute_stats", "Compute statistical summaries", TRUE),
            checkboxInput("confidence_regions", "Include confidence regions", FALSE),
            numericInput("confidence_level", "Confidence Level:", 0.95, min = 0.8, max = 0.99, step = 0.01),
            
            br(),
            actionButton("run_shape_analysis", "Run Shape Analysis", 
                        class = "btn-success btn-lg", icon = icon("play"),
                        style = "width: 100%"),
            
            br(), br(),
            conditionalPanel("output.shape_analysis_complete",
              downloadButton("download_shape_results", "Download Results", 
                           class = "btn-info", style = "width: 100%")
            )
          ),
          
          # Results Display
          box(
            title = "Analysis Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.shape_analysis_complete",
              div(style = "text-align: center; padding: 50px;",
                h4("Upload shape files and configure analysis settings to begin"),
                icon("shapes", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.shape_analysis_complete",
              tabsetPanel(
                tabPanel("PCA Plot",
                  withSpinner(plotlyOutput("pca_plot", height = "500px"))
                ),
                tabPanel("Scree Plot",
                  withSpinner(plotOutput("scree_plot", height = "500px"))
                ),
                tabPanel("Loadings",
                  withSpinner(DT::dataTableOutput("loadings_table"))
                ),
                tabPanel("Summary Stats",
                  withSpinner(verbatimTextOutput("analysis_summary"))
                ),
                tabPanel("Diagnostic Plots",
                  withSpinner(plotOutput("diagnostic_plots", height = "600px"))
                )
              )
            )
          )
        )
      ),
      
      # Morphometric Overview Tab ----
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Overview Plot Configuration", status = "primary", solidHeader = TRUE,
            width = 4,
            
            h4("Data Selection"),
            selectInput("overview_data", "Select Dataset:",
                       choices = c("Shape Analysis Results" = "shape_results",
                                  "Uploaded Data File" = "uploaded_data")),
            
            conditionalPanel("input.overview_data == 'uploaded_data'",
              selectInput("overview_columns", "Select Columns:", 
                         choices = NULL, multiple = TRUE)
            ),
            
            h4("Plot Configuration"),
            selectInput("panel_layout", "Panel Layout:",
              choices = c("2x2 Grid" = "2x2", "3x2 Grid" = "3x2", 
                         "4x2 Grid" = "4x2", "Custom" = "custom")),
            
            conditionalPanel("input.panel_layout == 'custom'",
              numericInput("custom_rows", "Rows:", 2, min = 1, max = 6),
              numericInput("custom_cols", "Columns:", 3, min = 1, max = 6)
            ),
            
            h4("Visualization Options"),
            checkboxInput("show_hull", "Show convex hulls", TRUE),
            checkboxInput("show_contours", "Show density contours", FALSE),
            checkboxInput("show_boxplots", "Show marginal boxplots", FALSE),
            
            selectInput("color_palette", "Color Palette:",
              choices = c("Default" = "default", "Viridis" = "viridis",
                         "RdYlBu" = "RdYlBu", "Set1" = "Set1", "Dark2" = "Dark2")),
            
            colourInput("background_color", "Background Color:", "#ffffff"),
            
            h4("Statistical Options"),
            checkboxInput("show_ellipses", "Show confidence ellipses", FALSE),
            numericInput("ellipse_confidence", "Ellipse Confidence:", 0.95, 
                        min = 0.8, max = 0.99, step = 0.01),
            
            br(),
            actionButton("generate_overview", "Generate Overview", 
                        class = "btn-success btn-lg", icon = icon("chart-area"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Morphometric Overview", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.overview_generated",
              div(style = "text-align: center; padding: 50px;",
                h4("Configure settings and click 'Generate Overview' to create plots"),
                icon("chart-area", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.overview_generated",
              withSpinner(plotOutput("overview_plot", height = "700px")),
              br(),
              fluidRow(
                column(6,
                  downloadButton("download_overview", "Download Plot", class = "btn-info")
                ),
                column(6,
                  actionButton("customize_overview", "Customize Plot", class = "btn-warning")
                )
              )
            )
          )
        )
      ),
      
      # Shape Plotting Tab ----
      tabItem(tabName = "shape_plot",
        fluidRow(
          box(
            title = "Shape Plot Configuration", status = "primary", solidHeader = TRUE,
            width = 4,
            
            h4("Plot Type"),
            radioButtons("plot_type", NULL,
              choices = c("Scatter Plot" = "scatter",
                         "PCA Biplot" = "biplot",
                         "Shape Reconstruction" = "reconstruction")),
            
            h4("Axis Selection"),
            conditionalPanel("input.plot_type == 'scatter' || input.plot_type == 'biplot'",
              selectInput("x_axis", "X-Axis Component:", choices = NULL),
              selectInput("y_axis", "Y-Axis Component:", choices = NULL)
            ),
            
            h4("Grouping & Colors"),
            selectInput("color_by", "Color By:", choices = c("None" = "none")),
            selectInput("shape_by", "Shape By:", choices = c("None" = "none")),
            
            h4("Statistical Overlays"),
            checkboxInput("add_hulls", "Add convex hulls", FALSE),
            checkboxInput("add_ellipses", "Add confidence ellipses", FALSE),
            checkboxInput("add_centroid", "Show group centroids", FALSE),
            checkboxInput("add_loadings", "Show variable loadings", FALSE),
            
            h4("Appearance"),
            sliderInput("point_size", "Point Size:", min = 0.5, max = 5, value = 2, step = 0.1),
            sliderInput("point_alpha", "Point Transparency:", min = 0.1, max = 1, value = 0.7, step = 0.1),
            
            selectInput("theme_choice", "Plot Theme:",
              choices = c("Classic" = "classic", "Minimal" = "minimal",
                         "Publication" = "publication", "Dark" = "dark")),
            
            br(),
            actionButton("create_shape_plot", "Create Plot", 
                        class = "btn-success btn-lg", icon = icon("scatter-chart"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Shape Plot Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.shape_plot_created",
              div(style = "text-align: center; padding: 50px;",
                h4("Configure plot settings and click 'Create Plot' to generate visualization"),
                icon("scatter-chart", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.shape_plot_created",
              tabsetPanel(
                tabPanel("Interactive Plot",
                  withSpinner(plotlyOutput("shape_plot_interactive", height = "600px"))
                ),
                tabPanel("Static Plot",
                  withSpinner(plotOutput("shape_plot_static", height = "600px"))
                ),
                tabPanel("Plot Statistics",
                  withSpinner(verbatimTextOutput("plot_statistics"))
                )
              ),
              br(),
              fluidRow(
                column(4,
                  downloadButton("download_shape_plot", "Download Plot", class = "btn-info")
                ),
                column(4,
                  numericInput("plot_width", "Width (inches):", 8, min = 4, max = 20)
                ),
                column(4,
                  numericInput("plot_height", "Height (inches):", 6, min = 3, max = 15)
                )
              )
            )
          )
        )
      ),
      
      # Clustering Tab ----
      tabItem(tabName = "clustering",
        fluidRow(
          box(
            title = "Clustering Analysis", status = "primary", solidHeader = TRUE,
            width = 4,
            
            h4("Clustering Method"),
            selectInput("cluster_method", "Algorithm:",
              choices = c("K-means" = "kmeans",
                         "Hierarchical" = "hierarchical",
                         "Gaussian Mixture" = "gaussian_mixture",
                         "DBSCAN" = "dbscan",
                         "PAM" = "pam")),
            
            conditionalPanel("input.cluster_method == 'kmeans'",
              numericInput("k_clusters", "Number of Clusters:", 3, min = 2, max = 20)
            ),
            
            conditionalPanel("input.cluster_method == 'hierarchical'",
              selectInput("linkage_method", "Linkage Method:",
                choices = c("Complete" = "complete", "Average" = "average",
                           "Single" = "single", "Ward" = "ward.D2"))
            ),
            
            conditionalPanel("input.cluster_method == 'dbscan'",
              numericInput("eps", "Epsilon (eps):", 0.5, min = 0.1, max = 5, step = 0.1),
              numericInput("min_pts", "Minimum Points:", 5, min = 3, max = 20)
            ),
            
            h4("Data Preprocessing"),
            checkboxInput("scale_before_cluster", "Scale data before clustering", TRUE),
            checkboxInput("remove_outliers", "Remove outliers", FALSE),
            
            conditionalPanel("input.remove_outliers",
              numericInput("outlier_threshold", "Outlier Threshold (SD):", 3, min = 1, max = 5)
            ),
            
            h4("Validation & Quality"),
            checkboxInput("compute_silhouette", "Compute silhouette scores", TRUE),
            checkboxInput("cross_validate", "Cross-validate clustering", FALSE),
            
            br(),
            actionButton("run_clustering", "Run Clustering Analysis", 
                        class = "btn-success btn-lg", icon = icon("project-diagram"),
                        style = "width: 100%"),
            
            br(), br(),
            conditionalPanel("output.clustering_complete",
              actionButton("optimize_clusters", "Optimize Cluster Number", 
                          class = "btn-warning", style = "width: 100%")
            )
          ),
          
          box(
            title = "Clustering Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.clustering_complete",
              div(style = "text-align: center; padding: 50px;",
                h4("Select clustering method and run analysis"),
                icon("project-diagram", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.clustering_complete",
              tabsetPanel(
                tabPanel("Cluster Plot",
                  withSpinner(plotlyOutput("cluster_plot", height = "500px"))
                ),
                tabPanel("Dendrogram",
                  withSpinner(plotOutput("dendrogram", height = "500px"))
                ),
                tabPanel("Silhouette Plot",
                  withSpinner(plotOutput("silhouette_plot", height = "400px"))
                ),
                tabPanel("Cluster Statistics",
                  withSpinner(DT::dataTableOutput("cluster_stats"))
                ),
                tabPanel("Validation Metrics",
                  withSpinner(verbatimTextOutput("cluster_validation"))
                )
              )
            )
          )
        )
      ),
      
      # Image Processing Tabs ----
      
      # Format Conversion
      tabItem(tabName = "convert",
        fluidRow(
          box(
            title = "Image Format Conversion", status = "primary", solidHeader = TRUE,
            width = 4,
            
            fileInput("convert_files", "Select Images to Convert",
                     multiple = TRUE,
                     accept = c(".jpg", ".jpeg", ".png", ".tiff", ".bmp")),
            
            h4("Conversion Settings"),
            selectInput("target_format_conv", "Target Format:",
              choices = c("PNG" = "png", "JPEG" = "jpg", "TIFF" = "tiff")),
            
            conditionalPanel("input.target_format_conv == 'jpg'",
              sliderInput("jpeg_quality", "JPEG Quality:", min = 10, max = 100, value = 90)
            ),
            
            h4("Processing Options"),
            checkboxInput("resize_images", "Resize images", FALSE),
            conditionalPanel("input.resize_images",
              numericInput("target_width", "Target Width (px):", 800, min = 100, max = 5000),
              numericInput("target_height", "Target Height (px):", 600, min = 100, max = 5000),
              checkboxInput("maintain_aspect", "Maintain aspect ratio", TRUE)
            ),
            
            checkboxInput("batch_convert", "Enable batch processing", TRUE),
            textInput("output_prefix", "Output filename prefix:", "converted_"),
            
            br(),
            actionButton("start_conversion", "Start Conversion", 
                        class = "btn-success", icon = icon("exchange-alt"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Conversion Progress", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.conversion_started",
              div(style = "text-align: center; padding: 50px;",
                h4("Select images and configure settings to begin conversion"),
                icon("exchange-alt", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.conversion_started",
              h4("Conversion Status"),
              withSpinner(verbatimTextOutput("conversion_status")),
              
              br(),
              h4("Converted Files"),
              withSpinner(DT::dataTableOutput("converted_files_table")),
              
              br(),
              downloadButton("download_converted", "Download All Converted Files", 
                           class = "btn-info")
            )
          )
        )
      ),
      
      # Image Cropping
      tabItem(tabName = "cropping",
        fluidRow(
          box(
            title = "Image Cropping Tools", status = "primary", solidHeader = TRUE,
            width = 4,
            
            fileInput("crop_files", "Select Images to Crop",
                     multiple = TRUE,
                     accept = c(".jpg", ".jpeg", ".png", ".tiff", ".bmp")),
            
            h4("Cropping Method"),
            radioButtons("crop_method", NULL,
              choices = c("Right Half" = "right_half",
                         "Left Half" = "left_half",
                         "Custom Rectangle" = "custom",
                         "Center Square" = "center_square")),
            
            conditionalPanel("input.crop_method == 'custom'",
              h5("Custom Crop Area (% of image)"),
              sliderInput("crop_left", "Left:", min = 0, max = 100, value = 25),
              sliderInput("crop_right", "Right:", min = 0, max = 100, value = 75),
              sliderInput("crop_top", "Top:", min = 0, max = 100, value = 25),
              sliderInput("crop_bottom", "Bottom:", min = 0, max = 100, value = 75)
            ),
            
            conditionalPanel("input.crop_method == 'center_square'",
              sliderInput("square_size", "Square Size (% of min dimension):", 
                         min = 10, max = 100, value = 80)
            ),
            
            h4("Processing Options"),
            checkboxInput("preserve_aspect_crop", "Preserve aspect ratio", FALSE),
            checkboxInput("auto_detect_content", "Auto-detect content area", FALSE),
            
            br(),
            actionButton("start_cropping", "Start Cropping", 
                        class = "btn-success", icon = icon("crop"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Cropping Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.cropping_started",
              div(style = "text-align: center; padding: 50px;",
                h4("Upload images and configure cropping settings"),
                icon("crop", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.cropping_started",
              tabsetPanel(
                tabPanel("Preview",
                  h4("Cropping Preview"),
                  withSpinner(plotOutput("crop_preview", height = "400px"))
                ),
                tabPanel("Results",
                  h4("Cropped Images"),
                  withSpinner(DT::dataTableOutput("cropped_files_table")),
                  br(),
                  downloadButton("download_cropped", "Download Cropped Images", 
                               class = "btn-info")
                ),
                tabPanel("Statistics",
                  withSpinner(verbatimTextOutput("crop_statistics"))
                )
              )
            )
          )
        )
      ),
      
      # Image Splitting
      tabItem(tabName = "splitting",
        fluidRow(
          box(
            title = "Image Splitting Tools", status = "primary", solidHeader = TRUE,
            width = 4,
            
            fileInput("split_files", "Select Images to Split",
                     multiple = TRUE,
                     accept = c(".jpg", ".jpeg", ".png", ".tiff", ".bmp")),
            
            h4("Split Method"),
            selectInput("split_method", "Split Type:",
              choices = c("Horizontal (Left/Right)" = "horizontal",
                         "Vertical (Top/Bottom)" = "vertical",
                         "Quadrants (2x2)" = "quadrants",
                         "Custom Grid" = "custom_grid")),
            
            conditionalPanel("input.split_method == 'horizontal'",
              sliderInput("horizontal_ratio", "Left/Right Ratio:", 
                         min = 0.1, max = 0.9, value = 0.5, step = 0.1)
            ),
            
            conditionalPanel("input.split_method == 'vertical'",
              sliderInput("vertical_ratio", "Top/Bottom Ratio:", 
                         min = 0.1, max = 0.9, value = 0.5, step = 0.1)
            ),
            
            conditionalPanel("input.split_method == 'custom_grid'",
              numericInput("grid_rows", "Number of Rows:", 2, min = 2, max = 8),
              numericInput("grid_cols", "Number of Columns:", 2, min = 2, max = 8)
            ),
            
            h4("Output Options"),
            checkboxInput("add_overlap", "Add overlap between sections", FALSE),
            conditionalPanel("input.add_overlap",
              numericInput("overlap_pixels", "Overlap (pixels):", 10, min = 0, max = 100)
            ),
            
            textInput("split_prefix", "Output filename prefix:", "split_"),
            
            br(),
            actionButton("start_splitting", "Start Splitting", 
                        class = "btn-success", icon = icon("th"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Splitting Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.splitting_started",
              div(style = "text-align: center; padding: 50px;",
                h4("Configure splitting method and process images"),
                icon("th", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.splitting_started",
              h4("Split Images Overview"),
              withSpinner(plotOutput("split_overview", height = "500px")),
              
              br(),
              h4("Generated Files"),
              withSpinner(DT::dataTableOutput("split_files_table")),
              
              br(),
              downloadButton("download_split", "Download Split Images", class = "btn-info")
            )
          )
        )
      ),
      
      # Shape Completion
      tabItem(tabName = "completion",
        fluidRow(
          box(
            title = "Shape Completion Tools", status = "primary", solidHeader = TRUE,
            width = 4,
            
            fileInput("completion_files", "Select Half-Shapes to Complete",
                     multiple = TRUE,
                     accept = c(".jpg", ".jpeg", ".png", ".tiff", ".bmp")),
            
            h4("Completion Method"),
            radioButtons("completion_method", NULL,
              choices = c("Horizontal Reflection" = "horizontal",
                         "Vertical Reflection" = "vertical",
                         "Auto-detect Symmetry" = "auto_detect",
                         "Statistical Completion" = "statistical")),
            
            h4("Processing Options"),
            checkboxInput("smooth_boundaries", "Smooth reflection boundaries", TRUE),
            conditionalPanel("input.smooth_boundaries",
              sliderInput("smoothing_radius", "Smoothing Radius:", 
                         min = 1, max = 10, value = 3)
            ),
            
            checkboxInput("validate_symmetry", "Validate symmetry assumption", TRUE),
            conditionalPanel("input.validate_symmetry",
              sliderInput("symmetry_threshold", "Symmetry Threshold:", 
                         min = 0.5, max = 1.0, value = 0.8, step = 0.05)
            ),
            
            h4("Output Options"),
            checkboxInput("preserve_original", "Keep original half", TRUE),
            selectInput("output_format_completion", "Output Format:",
              choices = c("PNG" = "png", "JPEG" = "jpg", "TIFF" = "tiff")),
            
            br(),
            actionButton("start_completion", "Complete Shapes", 
                        class = "btn-success", icon = icon("puzzle-piece"),
                        style = "width: 100%")
          ),
          
          box(
            title = "Shape Completion Results", status = "success", solidHeader = TRUE,
            width = 8,
            
            conditionalPanel("!output.completion_started",
              div(style = "text-align: center; padding: 50px;",
                h4("Upload half-shapes and configure completion method"),
                icon("puzzle-piece", style = "font-size: 48px; color: #ccc;")
              )
            ),
            
            conditionalPanel("output.completion_started",
              tabsetPanel(
                tabPanel("Before/After",
                  h4("Completion Preview"),
                  withSpinner(plotOutput("completion_preview", height = "500px"))
                ),
                tabPanel("Validation",
                  h4("Symmetry Validation"),
                  withSpinner(plotOutput("symmetry_validation", height = "400px")),
                  withSpinner(verbatimTextOutput("symmetry_statistics"))
                ),
                tabPanel("Results",
                  h4("Completed Shapes"),
                  withSpinner(DT::dataTableOutput("completed_files_table")),
                  br(),
                  downloadButton("download_completed", "Download Completed Shapes", 
                               class = "btn-info")
                )
              )
            )
          )
        )
      )

# Server Logic
server <- function(input, output, session) {
  
  # Reactive Values ----
  values <- reactiveValues(
    shape_files = NULL,
    data_file = NULL,
    shape_analysis_results = NULL,
    clustering_results = NULL,
    hull_results = NULL,
    current_plots = list(),
    session_data = list(),
    batch_status = NULL
  )
  
  # Data Upload Handlers ----
  
  # Shape files upload
  observeEvent(input$shape_files, {
    req(input$shape_files)
    values$shape_files <- input$shape_files
    
    # Update UI indicators
    output$shape_files_uploaded <- reactive({ !is.null(values$shape_files) })
    outputOptions(output, "shape_files_uploaded", suspendWhenHidden = FALSE)
    
    showNotification(paste("Uploaded", nrow(input$shape_files), "shape files"), type = "success")
  })
  
  # Data file upload
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      ext <- tools::file_ext(input$data_file$datapath)
      if (ext == "csv") {
        data <- read.csv(input$data_file$datapath, header = input$has_header, sep = input$separator)
      } else if (ext %in% c("xlsx", "xls")) {
        data <- readxl::read_excel(input$data_file$datapath)
      }
      
      values$data_file <- data
      
      # Update column choices
      col_names <- names(data)
      updateSelectInput(session, "id_column", choices = c("None" = "", col_names))
      updateSelectInput(session, "group_column", choices = c("None" = "", col_names))
      updateSelectInput(session, "analysis_columns", choices = col_names)
      updateSelectInput(session, "overview_columns", choices = col_names)
      updateSelectInput(session, "color_by", choices = c("None" = "none", col_names))
      updateSelectInput(session, "shape_by", choices = c("None" = "none", col_names))
      
      output$data_file_uploaded <- reactive({ TRUE })
      outputOptions(output, "data_file_uploaded", suspendWhenHidden = FALSE)
      
      showNotification("Data file uploaded successfully", type = "success")
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # File preview tables
  output$shape_files_table <- DT::renderDataTable({
    req(values$shape_files)
    
    file_info <- data.frame(
      Filename = values$shape_files$name,
      Size = paste(round(values$shape_files$size / 1024, 1), "KB"),
      Type = tools::file_ext(values$shape_files$name),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(file_info, options = list(pageLength = 5))
  })
  
  output$data_preview <- DT::renderDataTable({
    req(values$data_file)
    
    DT::datatable(
      values$data_file,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # Shape Analysis Module ----
  
  observeEvent(input$run_shape_analysis, {
    req(values$shape_files)
    
    withProgress(message = "Running shape analysis...", value = 0, {
      
      tryCatch({
        # Prepare file paths
        file_paths <- values$shape_files$datapath
        names(file_paths) <- values$shape_files$name
        
        incProgress(0.2, detail = "Processing images...")
        
        # Configure analysis options
        analysis_options <- list(
          method = input$analysis_method,
          efa_harmonics = input$efa_harmonics,
          pca_components = input$pca_components,
          normalize = input$efa_normalize,
          scale = input$pca_scale,
          center = input$pca_center,
          batch_process = input$batch_process,
          batch_size = input$batch_size
        )
        
        incProgress(0.3, detail = "Running analysis...")
        
        # Call shape_analysis function (placeholder - replace with actual function)
        # results <- shape_analysis(file_paths, analysis_options)
        
        # For demonstration, create mock results
        n_samples <- length(file_paths)
        n_components <- min(input$pca_components, n_samples - 1)
        
        mock_results <- list(
          pca_scores = matrix(rnorm(n_samples * n_components), ncol = n_components),
          loadings = matrix(rnorm(n_components * 10), ncol = n_components),
          variance_explained = cumsum(seq(0.3, 0.05, length.out = n_components)),
          specimen_names = values$shape_files$name,
          analysis_summary = list(
            method = input$analysis_method,
            n_specimens = n_samples,
            n_components = n_components
          )
        )
        
        colnames(mock_results$pca_scores) <- paste0("PC", 1:n_components)
        rownames(mock_results$pca_scores) <- mock_results$specimen_names
        
        incProgress(0.8, detail = "Finalizing results...")
        
        values$shape_analysis_results <- mock_results
        
        # Update UI choices
        pc_choices <- paste0("PC", 1:n_components)
        updateSelectInput(session, "x_axis", choices = pc_choices, selected = "PC1")
        updateSelectInput(session, "y_axis", choices = pc_choices, selected = "PC2")
        
        output$shape_analysis_complete <- reactive({ TRUE })
        outputOptions(output, "shape_analysis_complete", suspendWhenHidden = FALSE)
        
        incProgress(1, detail = "Complete!")
        
        showNotification("Shape analysis completed successfully!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Analysis failed:", e$message), type = "error")
      })
    })
  })
  
  # Shape analysis outputs
  output$pca_plot <- renderPlotly({
    req(values$shape_analysis_results)
    
    results <- values$shape_analysis_results
    df <- as.data.frame(results$pca_scores)
    df$specimen <- rownames(df)
    
    # Add grouping if available
    if (!is.null(values$data_file) && input$color_by != "none") {
      # Simple matching by name (in real implementation, use proper mapping)
      df$group <- sample(c("Group A", "Group B", "Group C"), nrow(df), replace = TRUE)
    }
    
    p <- plot_ly(df, x = ~PC1, y = ~PC2, text = ~specimen,
                type = "scatter", mode = "markers",
                marker = list(size = 8)) %>%
      layout(
        title = "PCA Plot",
        xlab = list(title = paste0("PC1 (", round(results$variance_explained[1] * 100, 1), "%)")),
        ylab = list(title = paste0("PC2 (", round((results$variance_explained[2] - results$variance_explained[1]) * 100, 1), "%)"))
      )
    
    if (exists("group", df)) {
      p <- p %>% add_markers(color = ~group)
    }
    
    p
  })
  
  output$scree_plot <- renderPlot({
    req(values$shape_analysis_results)
    
    results <- values$shape_analysis_results
    var_exp <- diff(c(0, results$variance_explained))
    
    df <- data.frame(
      PC = 1:length(var_exp),
      Variance = var_exp * 100
    )
    
    ggplot(df, aes(x = PC, y = Variance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_line(group = 1, color = "red") +
      geom_point(color = "red") +
      labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained (%)") +
      theme_minimal()
  })
  
  output$loadings_table <- DT::renderDataTable({
    req(values$shape_analysis_results)
    
    loadings_df <- as.data.frame(values$shape_analysis_results$loadings)
    colnames(loadings_df) <- paste0("PC", 1:ncol(loadings_df))
    loadings_df$Variable <- paste0("Var", 1:nrow(loadings_df))
    
    DT::datatable(loadings_df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      DT::formatRound(columns = 1:(ncol(loadings_df)-1), digits = 4)
  })
  
  output$analysis_summary <- renderText({
    req(values$shape_analysis_results)
    
    results <- values$shape_analysis_results
    summary_text <- paste(
      "Shape Analysis Summary",
      "=====================",
      paste("Method:", results$analysis_summary$method),
      paste("Number of specimens:", results$analysis_summary$n_specimens),
      paste("Number of components:", results$analysis_summary$n_components),
      paste("Total variance explained:", round(max(results$variance_explained) * 100, 1), "%"),
      "",
      "Component-wise variance:",
      paste(paste0("PC", 1:length(results$variance_explained), ": ", 
                   round(diff(c(0, results$variance_explained)) * 100, 2), "%"), collapse = "\n"),
      sep = "\n"
    )
    
    summary_text
  })
  
  # Overview Plot Module ----
  
  observeEvent(input$generate_overview, {
    withProgress(message = "Generating overview plots...", value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "Preparing data...")
        
        # Mock overview plot generation
        # In real implementation: results <- Haug_overview(...)
        
        incProgress(0.8, detail = "Creating plots...")
        
        output$overview_generated <- reactive({ TRUE })
        outputOptions(output, "overview_generated", suspendWhenHidden = FALSE)
        
        incProgress(1, detail = "Complete!")
        
        showNotification("Overview plots generated successfully!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Overview generation failed:", e$message), type = "error")
      })
    })
  })
  
  output$overview_plot <- renderPlot({
    req(values$shape_analysis_results)
    
    # Mock overview plot
    par(mfrow = c(2, 2))
    
    # Plot 1: PCA scores
    results <- values$shape_analysis_results
    plot(results$pca_scores[,1], results$pca_scores[,2], 
         main = "PCA Scores", xlab = "PC1", ylab = "PC2",
         pch = 16, col = rainbow(nrow(results$pca_scores)))
    
    # Plot 2: Variance explained
    var_exp <- diff(c(0, results$variance_explained))
    barplot(var_exp * 100, names.arg = paste0("PC", 1:length(var_exp)),
            main = "Variance Explained", ylab = "Percentage")
    
    # Plot 3: Loadings
    matplot(t(results$loadings[,1:2]), type = "l", 
            main = "Loadings PC1-PC2", ylab = "Loading")
    legend("topright", legend = c("PC1", "PC2"), col = 1:2, lty = 1:2)
    
    # Plot 4: Mock shape plot
    plot(1:10, rnorm(10), type = "b", main = "Shape Variation", 
         xlab = "Shape Index", ylab = "Value")
    
  }, height = 700)
  
  # Clustering Module ----
  
  observeEvent(input$run_clustering, {
    req(values$shape_analysis_results)
    
    withProgress(message = "Running clustering analysis...", value = 0, {
      
      tryCatch({
        incProgress(0.2, detail = "Preparing data...")
        
        data_for_clustering <- values$shape_analysis_results$pca_scores
        
        if (input$scale_before_cluster) {
          data_for_clustering <- scale(data_for_clustering)
        }
        
        incProgress(0.5, detail = "Performing clustering...")
        
        # Mock clustering (replace with actual cluster_plot function)
        if (input$cluster_method == "kmeans") {
          cluster_result <- kmeans(data_for_clustering, centers = input$k_clusters)
          clusters <- cluster_result$cluster
        } else {
          # Mock other methods
          clusters <- sample(1:3, nrow(data_for_clustering), replace = TRUE)
        }
        
        mock_clustering_results <- list(
          clusters = clusters,
          method = input$cluster_method,
          n_clusters = length(unique(clusters)),
          silhouette_avg = runif(1, 0.3, 0.8)
        )
        
        values$clustering_results <- mock_clustering_results
        
        output$clustering_complete <- reactive({ TRUE })
        outputOptions(output, "clustering_complete", suspendWhenHidden = FALSE)
        
        incProgress(1, detail = "Complete!")
        
        showNotification("Clustering analysis completed!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Clustering failed:", e$message), type = "error")
      })
    })
  })
  
  output$cluster_plot <- renderPlotly({
    req(values$clustering_results, values$shape_analysis_results)
    
    results <- values$shape_analysis_results
    clusters <- values$clustering_results$clusters
    
    df <- as.data.frame(results$pca_scores)
    df$cluster <- as.factor(clusters)
    df$specimen <- rownames(df)
    
    plot_ly(df, x = ~PC1, y = ~PC2, color = ~cluster, text = ~specimen,
           type = "scatter", mode = "markers",
           marker = list(size = 10)) %>%
      layout(title = paste("Clustering Results -", values$clustering_results$method))
  })
  
  output$cluster_validation <- renderText({
    req(values$clustering_results)
    
    paste(
      "Clustering Validation Results",
      "============================",
      paste("Method:", values$clustering_results$method),
      paste("Number of clusters:", values$clustering_results$n_clusters),
      paste("Average silhouette score:", round(values$clustering_results$silhouette_avg, 3)),
      sep = "\n"
    )
  })
  
  # Session Management ----
  
  observeEvent(input$save_session, {
    req(input$session_name)
    
    session_data <- list(
      shape_files = values$shape_files,
      data_file = values$data_file,
      shape_analysis_results = values$shape_analysis_results,
      clustering_results = values$clustering_results,
      timestamp = Sys.time()
    )
    
    tryCatch({
      filename <- paste0(make.names(input$session_name), "_", format(Sys.Date(), "%Y%m%d"), ".rds")
      # In real implementation, save to user-specified location
      # saveRDS(session_data, filename)
      
      showNotification(paste("Session saved as:", filename), type = "success")
    }, error = function(e) {
      showNotification(paste("Failed to save session:", e$message), type = "error")
    })
  })
  
  observeEvent(input$clear_all, {
    values$shape_files <- NULL
    values$data_file <- NULL
    values$shape_analysis_results <- NULL
    values$clustering_results <- NULL
    values$hull_results <- NULL
    
    # Reset UI indicators
    output$shape_files_uploaded <- reactive({ FALSE })
    output$data_file_uploaded <- reactive({ FALSE })
    output$shape_analysis_complete <- reactive({ FALSE })
    output$clustering_complete <- reactive({ FALSE })
    
    showNotification("All data cleared", type = "warning")
  })
  
  output$session_status <- renderText({
    status_items <- c()
    
    if (!is.null(values$shape_files)) {
      status_items <- c(status_items, paste("Shape files:", nrow(values$shape_files)))
    }
    
    if (!is.null(values$data_file)) {
      status_items <- c(status_items, paste("Data file:", ncol(values$data_file), "columns,", nrow(values$data_file), "rows"))
    }
    
    if (!is.null(values$shape_analysis_results)) {
      status_items <- c(status_items, "Shape analysis: Complete")
    }
    
    if (!is.null(values$clustering_results)) {
      status_items <- c(status_items, "Clustering: Complete")
    }
    
    if (length(status_items) == 0) {
      "No data loaded"
    } else {
      paste(status_items, collapse = "\n")
    }
  })
  
  # Download Handlers ----
  
  output$download_shape_results <- downloadHandler(
    filename = function() {
      paste0("shape_analysis_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$shape_analysis_results)
      write.csv(values$shape_analysis_results$pca_scores, file, row.names = TRUE)
    }
  )
  
  output$download_overview <- downloadHandler(
    filename = function() {
      paste0("morphometric_overview_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      # Recreate the overview plot
      # In real implementation, save the actual plot
      plot(1:10, type = "l", main = "Morphometric Overview")
      dev.off()
    }
  )
  
  # Initialize UI state
  output$shape_files_uploaded <- reactive({ FALSE })
  output$data_file_uploaded <- reactive({ FALSE })
  output$shape_analysis_complete <- reactive({ FALSE })
  output$clustering_complete <- reactive({ FALSE })
  output$overview_generated <- reactive({ FALSE })
  
  outputOptions(output, "shape_files_uploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "data_file_uploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "shape_analysis_complete", suspendWhenHidden = FALSE)
  outputOptions(output, "clustering_complete", suspendWhenHidden = FALSE)
  outputOptions(output, "overview_generated", suspendWhenHidden = FALSE)
}

# Run the Shiny Application ----
shinyApp(ui = ui, server = server)