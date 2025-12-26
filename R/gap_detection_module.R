#' Gap Detection Module UI
#'
#' @param id Module namespace ID
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
#'
#' @export

# Suppress R CMD check notes for ggplot2 NSE
utils::globalVariables(c("x", "y", "certainty", "group"))

gap_detection_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left column: Data input and parameters
      column(
        width = 6,
        
        # Data Input Box
        box(
          title = "1. Load PCA Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          
          helpText("Load a data frame containing PC scores (columns: PC1, PC2, PC3, ...)"),
          
          fileInput(
            ns("pca_data_file"),
            "Select PCA Scores File (CSV, Excel, RDS)",
            accept = c(".csv", ".xlsx", ".xls", ".rds"),
            placeholder = "No file selected"
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("data_loaded")),
            ns = ns,
            
            uiOutput(ns("data_preview_ui"))
          )
        ),
        
        # Parameters Box
        box(
          title = "2. Configure Parameters",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = NULL,
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("uncertainty"),
                "Measurement Uncertainty (%)",
                value = 5,
                min = 0,
                max = 50,
                step = 1
              ),
              
              numericInput(
                ns("grid_resolution"),
                "Grid Resolution",
                value = 150,
                min = 50,
                max = 500,
                step = 10
              ),
              
              numericInput(
                ns("monte_carlo_iterations"),
                "Monte Carlo Iterations",
                value = 100,
                min = 10,
                max = 1000,
                step = 10
              )
            ),
            
            column(
              width = 6,
              numericInput(
                ns("bootstrap_iterations"),
                "Bootstrap Iterations",
                value = 200,
                min = 10,
                max = 1000,
                step = 10
              ),
              
              numericInput(
                ns("max_pcs"),
                "Maximum PC to Analyze",
                value = 4,
                min = 2,
                max = 10,
                step = 1
              ),
              
              selectInput(
                ns("hull_type"),
                "Domain Hull Type",
                choices = c("Alpha Hull (Concave)" = "alpha",
                           "Convex Hull" = "convex"),
                selected = "alpha"
              )
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns("hull_buffer"),
                "Hull Buffer (%)",
                value = 5,
                min = 0,
                max = 20,
                step = 1
              ),
              
              selectInput(
                ns("uncertainty_type"),
                "Uncertainty Model",
                choices = c("Gaussian" = "gaussian",
                           "Uniform" = "uniform"),
                selected = "gaussian"
              )
            ),
            
            column(
              width = 6,
              selectInput(
                ns("occupancy_method"),
                "Occupancy Method",
                choices = c("Radius" = "radius",
                           "Kernel Density" = "kde"),
                selected = "radius"
              ),
              
              numericInput(
                ns("occupancy_radius"),
                "Occupancy Radius (× cell size)",
                value = 1.5,
                min = 0.5,
                max = 5,
                step = 0.1
              )
            )
          ),
          
          hr(),
          
          h5("Certainty Thresholds for Polygon Extraction"),
          fluidRow(
            column(
              width = 4,
              numericInput(
                ns("threshold_1"),
                "Threshold 1",
                value = 0.80,
                min = 0,
                max = 1,
                step = 0.05
              )
            ),
            column(
              width = 4,
              numericInput(
                ns("threshold_2"),
                "Threshold 2",
                value = 0.90,
                min = 0,
                max = 1,
                step = 0.05
              )
            ),
            column(
              width = 4,
              numericInput(
                ns("threshold_3"),
                "Threshold 3",
                value = 0.95,
                min = 0,
                max = 1,
                step = 0.05
              )
            )
          ),
          
          hr(),
          
          checkboxInput(
            ns("use_parallel"),
            "Use Parallel Processing (experimental)",
            value = FALSE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s']", ns("use_parallel")),
            ns = ns,
            
            numericInput(
              ns("n_cores"),
              "Number of Cores",
              value = 2,
              min = 1,
              max = parallel::detectCores(),
              step = 1
            )
          )
        ),
        
        # Execution Box
        box(
          title = "3. Run Analysis",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          
          h5("Output Settings"),
          
          checkboxInput(
            ns("auto_save"),
            "Automatically save results to file",
            value = TRUE
          ),
          
          uiOutput(ns("output_folder_ui")),
          
          hr(),
          
          actionButton(
            ns("run_analysis"),
            "Detect Gaps",
            icon = icon("play"),
            class = "btn-success btn-lg btn-block"
          ),
          
          br(),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_running")),
            ns = ns,
            
            div(
              style = "text-align: center;",
              h4("Analysis in progress..."),
              shinycssloaders::withSpinner(
                uiOutput(ns("progress_text")),
                type = 6,
                color = "#3c8dbc"
              )
            )
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_complete")),
            ns = ns,
            
            hr(),
            
            h4(icon("check-circle"), "Analysis Complete!"),
            
            uiOutput(ns("analysis_summary_ui")),
            
            br(),
            
            uiOutput(ns("saved_file_display")),
            
            downloadButton(
              ns("download_results_rds"),
              "Download Results (RDS)",
              class = "btn-primary"
            ),
            
            downloadButton(
              ns("download_summary_csv"),
              "Download Summary Table (CSV)",
              class = "btn-primary"
            ),
            
            br(), br(),
            
            actionButton(
              ns("clear_results"),
              "Clear Results",
              icon = icon("trash"),
              class = "btn-warning"
            )
          )
        )
      ),
      
      # Right column: Visualization and results
      column(
        width = 6,
        
        # Visualization Box
        box(
          title = "Gap Visualization",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = NULL,
          
          conditionalPanel(
            condition = sprintf("!output['%s']", ns("analysis_complete")),
            ns = ns,
            
            div(
              style = "text-align: center; padding: 50px;",
              icon("crosshairs", class = "fa-5x", style = "color: #ccc;"),
              h4("Run analysis to visualize gaps", style = "color: #999;")
            )
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_complete")),
            ns = ns,
            
            fluidRow(
              column(
                width = 6,
                uiOutput(ns("pc_pair_selector_ui"))
              ),
              column(
                width = 6,
                uiOutput(ns("certainty_threshold_selector_ui"))
              )
            ),
            
            hr(),
            
            plotOutput(
              ns("gap_plot"),
              height = "500px"
            ) %>%
              shinycssloaders::withSpinner(type = 4, color = "#3c8dbc")
          )
        ),
        
        # Results Table Box
        box(
          title = "Gap Metrics Table",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("analysis_complete")),
            ns = ns,
            
            DT::dataTableOutput(ns("gap_metrics_table"))
          )
        )
      )
    )
  )
}


#' Gap Detection Module Server
#'
#' @param id Module namespace ID
#'
#' @import shiny
#' @importFrom DT renderDataTable datatable
#'
#' @export
#' Gap Detection Module Server
#'
#' @param id Module namespace ID
#' @param pca_data Optional reactive containing PCA scores data frame. If provided,
#'   this data will be used instead of requiring file upload.
#'
#' @import shiny
#'
#' @export
gap_detection_server <- function(id, pca_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
      pca_data = NULL,
      gap_results = NULL,
      analysis_running = FALSE,
      analysis_complete = FALSE,
      saved_file_path = NULL
    )
    
    # Check for shinyFiles availability
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    
    # Setup directory chooser
    observeEvent(shinyfiles_ready(), {
      if (!isTRUE(shinyfiles_ready())) return()
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      shinyFiles::shinyDirChoose(
        input,
        id = "browse_folder",
        roots = roots,
        session = session
      )
    })
    
    # Handle folder selection
    observeEvent(input$browse_folder, {
      req(shinyfiles_ready())
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      sel <- try(shinyFiles::parseDirPath(roots, input$browse_folder), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) {
        updateTextInput(session, "output_folder", value = as.character(sel))
      }
    })
    
    # Load PCA data from file OR use provided reactive data
    observeEvent(input$pca_data_file, {
      req(input$pca_data_file)
      
      file_path <- input$pca_data_file$datapath
      file_ext <- tools::file_ext(input$pca_data_file$name)
      
      tryCatch({
        if (file_ext == "rds") {
          rv$pca_data <- readRDS(file_path)
        } else if (file_ext == "csv") {
          rv$pca_data <- read.csv(file_path, stringsAsFactors = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            showNotification("Package 'readxl' required for Excel files",
                           type = "error", duration = 5)
            return()
          }
          rv$pca_data <- as.data.frame(readxl::read_excel(file_path))
        } else {
          showNotification("Unsupported file format", type = "error", duration = 5)
          return()
        }
        
        # Validate data
        pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
        
        if (length(pc_cols) < 2) {
          showNotification(
            "Data must contain at least 2 columns named PC1, PC2, etc.",
            type = "error",
            duration = 8
          )
          rv$pca_data <- NULL
          return()
        }
        
        showNotification(
          sprintf("Loaded %d specimens with %d PCs from file", 
                  nrow(rv$pca_data), length(pc_cols)),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        rv$pca_data <- NULL
      })
    })
    
    # If external PCA data is provided via reactive, use it
    observe({
      if (!is.null(pca_data) && is.reactive(pca_data)) {
        external_data <- pca_data()
        if (!is.null(external_data)) {
          # Validate external data
          pc_cols <- grep("^PC[0-9]+$", colnames(external_data), value = TRUE)
          
          if (length(pc_cols) >= 2) {
            rv$pca_data <- external_data
            showNotification(
              sprintf("Using PCA scores from Shape Analysis: %d specimens with %d PCs", 
                      nrow(external_data), length(pc_cols)),
              type = "message",
              duration = 5
            )
          }
        }
      }
    })
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = 8
        )
        rv$pca_data <- NULL
      })
    })
    
    # Data loaded indicator
    output$data_loaded <- reactive({
      !is.null(rv$pca_data)
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    # Data preview UI
    output$data_preview_ui <- renderUI({
      req(rv$pca_data)
      
      pc_cols <- grep("^PC[0-9]+$", colnames(rv$pca_data), value = TRUE)
      
      tagList(
        hr(),
        h5(icon("check"), "Data Loaded Successfully"),
        p(sprintf("Specimens: %d | PC columns: %s", 
                 nrow(rv$pca_data), paste(pc_cols, collapse = ", "))),
        
        verbatimTextOutput(session$ns("data_head"))
      )
    })
    
    output$data_head <- renderPrint({
      req(rv$pca_data)
      head(rv$pca_data, 3)
    })
    
    # Output folder UI
    output$output_folder_ui <- renderUI({
      if (!isTRUE(input$auto_save)) {
        return(NULL)
      }
      
      tagList(
        fluidRow(
          column(
            width = 9,
            textInput(
              session$ns("output_folder"),
              "Output Folder",
              value = getwd(),
              placeholder = "Path to save results"
            )
          ),
          column(
            width = 3,
            br(),
            if (isTRUE(shinyfiles_ready())) {
              shinyFiles::shinyDirButton(
                session$ns("browse_folder"),
                "Browse...",
                title = "Select output folder",
                icon = icon("folder-open"),
                class = "btn-default"
              )
            } else {
              actionButton(
                session$ns("browse_folder_disabled"),
                "Browse...",
                icon = icon("folder-open"),
                class = "btn-default",
                disabled = TRUE
              )
            }
          )
        ),
        helpText("Results will be saved as: gap_results_YYYYMMDD_HHMMSS.rds")
      )
    })
    
    # Run analysis
    observeEvent(input$run_analysis, {
      req(rv$pca_data)
      
      # Validate inputs
      if (input$uncertainty <= 0 || input$uncertainty > 50) {
        showNotification("Uncertainty must be between 0 and 50%",
                        type = "error", duration = 5)
        return()
      }
      
      if (input$grid_resolution < 50 || input$grid_resolution > 500) {
        showNotification("Grid resolution must be between 50 and 500",
                        type = "error", duration = 5)
        return()
      }
      
      # Set analysis running
      rv$analysis_running <- TRUE
      rv$analysis_complete <- FALSE
      
      # Gather parameters
      certainty_thresholds <- c(
        input$threshold_1,
        input$threshold_2,
        input$threshold_3
      )
      certainty_thresholds <- sort(unique(certainty_thresholds))
      
      n_cores <- NULL
      if (input$use_parallel) {
        n_cores <- input$n_cores
      }
      
      # Run analysis with progress
      withProgress(message = "Detecting gaps in morphospace...", value = 0, {
        
        incProgress(0.1, detail = "Initializing...")
        
        tryCatch({
          
          results <- detect_morphospace_gaps(
            pca_scores = rv$pca_data,
            uncertainty = input$uncertainty / 100,
            grid_resolution = input$grid_resolution,
            monte_carlo_iterations = input$monte_carlo_iterations,
            bootstrap_iterations = input$bootstrap_iterations,
            certainty_thresholds = certainty_thresholds,
            max_pcs = input$max_pcs,
            hull_type = input$hull_type,
            hull_buffer = input$hull_buffer / 100,
            uncertainty_type = input$uncertainty_type,
            occupancy_method = input$occupancy_method,
            occupancy_radius = input$occupancy_radius,
            use_parallel = input$use_parallel,
            n_cores = n_cores,
            verbose = TRUE
          )
          
          incProgress(0.9, detail = "Finalizing...")
          
          rv$gap_results <- results
          rv$analysis_running <- FALSE
          rv$analysis_complete <- TRUE
          
          # Auto-save if enabled
          if (isTRUE(input$auto_save)) {
            output_folder <- input$output_folder
            
            # Validate output folder
            if (is.null(output_folder) || !nzchar(output_folder)) {
              output_folder <- getwd()
            }
            
            # Create directory if it doesn't exist
            if (!dir.exists(output_folder)) {
              tryCatch({
                dir.create(output_folder, recursive = TRUE)
              }, error = function(e) {
                showNotification(
                  paste("Could not create output folder:", e$message),
                  type = "warning",
                  duration = 5
                )
                output_folder <- getwd()
              })
            }
            
            # Generate filename with timestamp
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            filename <- sprintf("gap_results_%s.rds", timestamp)
            filepath <- file.path(output_folder, filename)
            
            # Save results
            tryCatch({
              saveRDS(results, filepath)
              rv$saved_file_path <- filepath
              
              showNotification(
                sprintf("Results saved to: %s", basename(filepath)),
                type = "message",
                duration = 8
              )
            }, error = function(e) {
              showNotification(
                paste("Error saving results:", e$message),
                type = "error",
                duration = 8
              )
              rv$saved_file_path <- NULL
            })
          }
          
          showNotification(
            sprintf("Analysis complete! Detected %d gap regions across %d PC pairs.",
                   nrow(results$summary_table), length(results$results)),
            type = "message",
            duration = 8
          )
          
        }, error = function(e) {
          rv$analysis_running <- FALSE
          rv$analysis_complete <- FALSE
          
          showNotification(
            paste("Analysis error:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
    
    # Analysis running indicator
    output$analysis_running <- reactive({
      rv$analysis_running
    })
    outputOptions(output, "analysis_running", suspendWhenHidden = FALSE)
    
    # Analysis complete indicator
    output$analysis_complete <- reactive({
      rv$analysis_complete
    })
    outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
    
    # Progress text
    output$progress_text <- renderUI({
      if (rv$analysis_running) {
        p("This may take several minutes depending on parameters...")
      }
    })
    
    # Analysis summary
    output$analysis_summary_ui <- renderUI({
      req(rv$gap_results)
      
      n_pairs <- length(rv$gap_results$results)
      n_gaps <- nrow(rv$gap_results$summary_table)
      
      tagList(
        p(sprintf("Analyzed %d PC pairs", n_pairs)),
        p(sprintf("Detected %d gap regions", n_gaps)),
        p(sprintf("Thresholds: %s", 
                 paste(rv$gap_results$parameters$certainty_thresholds, collapse = ", ")))
      )
    })
    
    # Display saved file path
    output$saved_file_path <- renderText({
      if (is.null(rv$saved_file_path)) {
        return("(Results not saved)")
      }
      rv$saved_file_path
    })
    
    # Saved file display UI
    output$saved_file_display <- renderUI({
      if (!isTRUE(input$auto_save) || is.null(rv$saved_file_path)) {
        return(NULL)
      }
      
      tagList(
        hr(),
        div(
          style = "padding: 10px; background-color: #d4edda; border: 1px solid #28a745; border-radius: 4px;",
          tags$strong(icon("check"), " Results saved to:"),
          br(),
          tags$code(rv$saved_file_path, style = "font-size: 11px; word-break: break-all;")
        )
      )
    })
    
    # PC pair selector
    output$pc_pair_selector_ui <- renderUI({
      req(rv$gap_results)
      
      pair_names <- names(rv$gap_results$results)
      
      selectInput(
        session$ns("selected_pc_pair"),
        "Select PC Pair",
        choices = pair_names,
        selected = pair_names[1]
      )
    })
    
    # Certainty threshold selector
    output$certainty_threshold_selector_ui <- renderUI({
      req(rv$gap_results)
      
      thresholds <- rv$gap_results$parameters$certainty_thresholds
      
      selectInput(
        session$ns("selected_threshold"),
        "Certainty Threshold",
        choices = thresholds,
        selected = thresholds[length(thresholds)]
      )
    })
    
    # Gap visualization plot
    output$gap_plot <- renderPlot({
      req(rv$gap_results, input$selected_pc_pair, input$selected_threshold)
      
      pair_result <- rv$gap_results$results[[input$selected_pc_pair]]
      threshold <- as.numeric(input$selected_threshold)
      
      # Create plot
      .plot_gap_result(pair_result, input$selected_pc_pair, threshold)
    })
    
    # Gap metrics table
    output$gap_metrics_table <- DT::renderDataTable({
      req(rv$gap_results)
      
      summary_table <- rv$gap_results$summary_table
      
      if (nrow(summary_table) == 0) {
        return(data.frame(Message = "No gaps detected"))
      }
      
      # Format numeric columns
      summary_table$area <- round(summary_table$area, 4)
      summary_table$mean_certainty <- round(summary_table$mean_certainty, 3)
      summary_table$max_certainty <- round(summary_table$max_certainty, 3)
      summary_table$gap_depth <- round(summary_table$gap_depth, 4)
      summary_table$centroid_x <- round(summary_table$centroid_x, 4)
      summary_table$centroid_y <- round(summary_table$centroid_y, 4)
      
      DT::datatable(
        summary_table,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(4, 'desc'))  # Sort by area descending
        ),
        rownames = FALSE
      )
    })
    
    # Download results RDS
    output$download_results_rds <- downloadHandler(
      filename = function() {
        paste0("morphospace_gaps_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        saveRDS(rv$gap_results, file)
      }
    )
    
    # Download summary CSV
    output$download_summary_csv <- downloadHandler(
      filename = function() {
        paste0("gap_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        write.csv(rv$gap_results$summary_table, file, row.names = FALSE)
      }
    )
    
    # Clear results
    observeEvent(input$clear_results, {
      rv$gap_results <- NULL
      rv$analysis_complete <- FALSE
      rv$analysis_running <- FALSE
      rv$saved_file_path <- NULL
      
      showNotification("Results cleared", type = "message", duration = 3)
    })
  })
}


#' Plot Gap Result for Single PC Pair
#'
#' @param pair_result Result object for one PC pair
#' @param pair_name Name of PC pair (e.g., "PC1-PC2")
#' @param threshold Certainty threshold to display
#'
#' @keywords internal
.plot_gap_result <- function(pair_result, pair_name, threshold) {
  
  # Extract data
  gap_certainty <- pair_result$gap_certainty
  grid_x <- pair_result$grid_x
  grid_y <- pair_result$grid_y
  
  # Create data frame for plotting
  gap_df <- expand.grid(x = grid_x, y = grid_y)
  gap_df$certainty <- as.vector(gap_certainty)
  
  # Filter out NA values
  gap_df <- gap_df[!is.na(gap_df$certainty), ]
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = gap_df,
      ggplot2::aes(x = x, y = y, fill = certainty),
      alpha = 0.8
    ) +
    ggplot2::scale_fill_gradient2(
      low = "white",
      mid = "yellow",
      high = "red",
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Gap\nCertainty"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = sprintf("Morphospace Gaps: %s", pair_name),
      subtitle = sprintf("Certainty threshold: %.2f", threshold),
      x = sub("-.*", "", pair_name),
      y = sub(".*-", "", pair_name)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12)
    )
  
  # Add gap polygons at threshold
  gap_polygons <- pair_result$gap_polygons
  
  if (nrow(gap_polygons) > 0) {
    gap_at_threshold <- gap_polygons[gap_polygons$threshold == threshold, ]
    
    if (nrow(gap_at_threshold) > 0) {
      # Convert sf to data frame for ggplot
      gap_coords_list <- lapply(seq_len(nrow(gap_at_threshold)), function(i) {
        coords <- sf::st_coordinates(gap_at_threshold[i, ])
        data.frame(
          x = coords[, 1],
          y = coords[, 2],
          group = i
        )
      })
      
      gap_coords <- do.call(rbind, gap_coords_list)
      
      p <- p +
        ggplot2::geom_polygon(
          data = gap_coords,
          ggplot2::aes(x = x, y = y, group = group),
          fill = NA,
          color = "black",
          size = 1.2
        )
    }
  }
  
  # Add domain hull
  hull_coords <- sf::st_coordinates(pair_result$domain_hull)
  hull_df <- data.frame(x = hull_coords[, 1], y = hull_coords[, 2])
  
  p <- p +
    ggplot2::geom_path(
      data = hull_df,
      ggplot2::aes(x = x, y = y),
      color = "blue",
      linetype = "dashed",
      size = 0.8
    )
  
  p
}
