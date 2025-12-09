#' Shape Reconstruction Module
#'
#' UI and server for interactive shape reconstruction from PCA models.
#' Allows loading reconstruction models and generating shapes from PC scores.
#'
#' @export
shape_reconstruction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Load Reconstruction Model",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          # Model file chooser
          uiOutput(ns("model_file_ui")),
          helpText("Select a reconstruction model RDS file (e.g., *_reconstruction_model.rds)"),
          
          actionButton(ns("load_model"), "Load Model", class = "btn-primary"),
          
          hr(),
          
          # Model information display
          uiOutput(ns("model_info_ui"))
        ),
        
        box(
          title = "Shape Reconstruction Parameters",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          # PC score inputs (dynamically generated based on loaded model)
          uiOutput(ns("pc_score_inputs_ui")),
          
          hr(),
          
          # Reconstruction options
          checkboxInput(ns("show_original"), "Show original shape (if available)", value = FALSE),
          numericInput(ns("plot_width"), "Plot width", value = 600, min = 200, max = 1200, step = 50),
          numericInput(ns("plot_height"), "Plot height", value = 600, min = 200, max = 1200, step = 50),
          
          hr(),
          
          actionButton(ns("reconstruct"), "Reconstruct Shape", class = "btn-success"),
          downloadButton(ns("download_coords"), "Download Coordinates (CSV)"),
          downloadButton(ns("download_plot"), "Download Plot (PNG)")
        ),
        
        box(
          title = "Reconstructed Shape",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          
          plotOutput(ns("shape_plot"), height = "auto"),
          br(),
          verbatimTextOutput(ns("reconstruction_info"))
        ),
        
        box(
          title = "Batch Reconstruction",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          
          helpText("Generate multiple shapes along PC axes for morphospace exploration."),
          
          selectInput(ns("batch_pc"), "PC axis to vary", choices = NULL),
          numericInput(ns("batch_min"), "Minimum value (SD units)", value = -3, step = 0.5),
          numericInput(ns("batch_max"), "Maximum value (SD units)", value = 3, step = 0.5),
          numericInput(ns("batch_steps"), "Number of steps", value = 7, min = 3, max = 20, step = 1),
          
          checkboxInput(ns("batch_hold_others"), "Hold other PCs at zero", value = TRUE),
          
          actionButton(ns("batch_reconstruct"), "Generate Batch", class = "btn-warning"),
          
          hr(),
          
          plotOutput(ns("batch_plot"), height = 800),
          downloadButton(ns("download_batch"), "Download Batch Grid (PNG)")
        )
      )
    )
  )
}

#' @export
shape_reconstruction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    loaded_model <- reactiveVal(NULL)
    reconstructed_shape <- reactiveVal(NULL)
    batch_shapes <- reactiveVal(NULL)
    
    # Package availability
    shinyfiles_ready <- reactiveVal(FALSE)
    momocs_ready <- reactiveVal(FALSE)
    
    # Try to ensure dependencies
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    
    observe({
      ready <- requireNamespace("Momocs", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("Momocs", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("Momocs", quietly = TRUE)
      }
      momocs_ready(isTRUE(ready))
    })
    
    # Model file chooser UI
    output$model_file_ui <- renderUI({
      if (isTRUE(shinyfiles_ready())) {
        tagList(
          shinyFiles::shinyFilesButton(
            ns("model_file_btn"), 
            label = "Choose reconstruction model file", 
            title = "Select RDS file",
            multiple = FALSE
          ),
          br(), br(),
          strong("Selected file: "), 
          textOutput(ns("model_file_selected"), inline = TRUE)
        )
      } else {
        textInput(ns("model_file_fallback"), "Model file path (.rds)", value = "")
      }
    })
    
    # Model file path reactive
    model_file_path <- reactiveVal("")
    
    # Setup file chooser
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
      
      shinyFiles::shinyFileChoose(
        input, 
        id = "model_file_btn", 
        roots = roots, 
        session = session,
        filetypes = c("rds", "RDS")
      )
    })
    
    # Handle file selection
    observeEvent(input$model_file_btn, {
      req(shinyfiles_ready())
      
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      
      sel <- try(shinyFiles::parseFilePaths(roots, input$model_file_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && nrow(sel) > 0) {
        model_file_path(as.character(sel$datapath[1]))
      }
    })
    
    # Fallback file path
    observe({
      if (!isTRUE(shinyfiles_ready()) && !is.null(input$model_file_fallback) && nzchar(input$model_file_fallback)) {
        model_file_path(input$model_file_fallback)
      }
    })
    
    # Display selected file
    output$model_file_selected <- renderText({
      path <- model_file_path()
      if (is.null(path) || !nzchar(path)) {
        "No file selected"
      } else {
        basename(path)
      }
    })
    
    # Load model
    observeEvent(input$load_model, {
      path <- model_file_path()
      
      if (is.null(path) || !nzchar(path)) {
        showNotification("Please select a model file first.", type = "warning")
        return()
      }
      
      if (!file.exists(path)) {
        showNotification("Model file does not exist.", type = "error")
        return()
      }
      
      if (!requireNamespace("Momocs", quietly = TRUE)) {
        showNotification("Package 'Momocs' is required but not available.", type = "error")
        return()
      }
      
      withProgress(message = "Loading reconstruction model...", value = 0.5, {
        model <- tryCatch({
          load_reconstruction_model(path, validate = TRUE, verbose = FALSE)
        }, error = function(e) {
          showNotification(paste("Failed to load model:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
        
        if (!is.null(model)) {
          loaded_model(model)
          showNotification("Model loaded successfully!", type = "message")
        }
      })
    })
    
    # Display model information
    output$model_info_ui <- renderUI({
      model <- loaded_model()
      req(model)
      
      tagList(
        tags$h4("Model Information", style = "color: #3c8dbc;"),
        tags$table(
          class = "table table-condensed",
          tags$tr(tags$td(tags$strong("Specimens:")), tags$td(model$parameters$n_specimens)),
          tags$tr(tags$td(tags$strong("Principal Components:")), tags$td(model$parameters$n_components)),
          tags$tr(tags$td(tags$strong("Harmonics:")), tags$td(model$parameters$harmonics)),
          tags$tr(tags$td(tags$strong("Normalization:")), tags$td(as.character(model$parameters$norm))),
          tags$tr(tags$td(tags$strong("Start Point:")), tags$td(model$parameters$start_point)),
          tags$tr(tags$td(tags$strong("Created:")), tags$td(format(model$metadata$created_date))),
          tags$tr(tags$td(tags$strong("Format Version:")), tags$td(model$metadata$format_version))
        ),
        tags$hr(),
        tags$h5("Variance Explained:"),
        tags$pre(style = "max-height: 150px; overflow-y: auto; font-size: 11px;", {
          if (!is.null(model$variance_explained)) {
            n_show <- min(10, length(model$variance_explained))
            lines <- sapply(1:n_show, function(i) {
              sprintf("PC%d: %.2f%%", i, model$variance_explained[i])
            })
            paste(lines, collapse = "\n")
          } else {
            "Variance information not available"
          }
        })
      )
    })
    
    # Generate PC score inputs dynamically
    output$pc_score_inputs_ui <- renderUI({
      model <- loaded_model()
      req(model)
      
      n_pcs <- min(10, model$parameters$n_components)  # Limit to first 10 PCs for UI
      
      inputs <- lapply(1:n_pcs, function(i) {
        var_pct <- if (!is.null(model$variance_explained) && i <= length(model$variance_explained)) {
          sprintf(" (%.1f%%)", model$variance_explained[i])
        } else {
          ""
        }
        
        numericInput(
          ns(paste0("pc", i)),
          paste0("PC", i, var_pct),
          value = 0,
          step = 0.1
        )
      })
      
      tagList(
        tags$h4("PC Scores (in SD units)"),
        helpText("Enter desired PC scores. 0 = mean shape, ±1 = one standard deviation from mean."),
        do.call(fluidRow, lapply(inputs, function(inp) column(width = 3, inp)))
      )
    })
    
    # Update batch PC choices when model is loaded
    observe({
      model <- loaded_model()
      req(model)
      
      n_pcs <- min(10, model$parameters$n_components)
      choices <- setNames(1:n_pcs, paste0("PC", 1:n_pcs))
      updateSelectInput(session, "batch_pc", choices = choices, selected = 1)
    })
    
    # Reconstruct shape
    observeEvent(input$reconstruct, {
      model <- loaded_model()
      
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }
      
      if (!requireNamespace("Momocs", quietly = TRUE)) {
        showNotification("Package 'Momocs' is required.", type = "error")
        return()
      }
      
      # Collect PC scores from inputs
      n_pcs <- min(10, model$parameters$n_components)
      pc_scores <- sapply(1:n_pcs, function(i) {
        input[[paste0("pc", i)]] %||% 0
      })
      names(pc_scores) <- paste0("PC", 1:n_pcs)
      
      withProgress(message = "Reconstructing shape...", value = 0.5, {
        shape <- tryCatch({
          .reconstruct_single_shape(model, pc_scores)
        }, error = function(e) {
          showNotification(paste("Reconstruction failed:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
        
        if (!is.null(shape)) {
          reconstructed_shape(list(coords = shape, pc_scores = pc_scores))
          showNotification("Shape reconstructed!", type = "message")
        }
      })
    })
    
    # Helper function to reconstruct a single shape
    .reconstruct_single_shape <- function(model, pc_scores) {
      # Ensure pc_scores has correct length
      if (length(pc_scores) > ncol(model$rotation)) {
        pc_scores <- pc_scores[1:ncol(model$rotation)]
      } else if (length(pc_scores) < ncol(model$rotation)) {
        # Pad with zeros
        full_scores <- rep(0, ncol(model$rotation))
        full_scores[1:length(pc_scores)] <- pc_scores
        pc_scores <- full_scores
      }
      
      # Reconstruct Fourier coefficients
      # fourier_coefs = center + (pc_scores × rotation)
      reconstructed_coefs <- model$center + as.vector(pc_scores %*% t(model$rotation))
      
      # Convert back to EFA coefficient structure
      # The efa_results object contains the structure we need
      efa_obj <- model$efa_results
      
      # Create a single-specimen version with reconstructed coefficients
      reconstructed_efa <- efa_obj
      reconstructed_efa$coe <- matrix(reconstructed_coefs, nrow = 1)
      rownames(reconstructed_efa$coe) <- "reconstructed"
      
      # Use inverse Fourier to get outline coordinates
      outline <- Momocs::efourier_i(reconstructed_efa, nb.h = NULL, nb.pts = 120)
      
      # Extract coordinates
      if (inherits(outline, "Out")) {
        coords <- outline$coo[[1]]
      } else {
        coords <- outline
      }
      
      return(coords)
    }
    
    # Helper operator
    `%||%` <- function(a, b) if (is.null(a)) b else a
    
    # Plot reconstructed shape
    output$shape_plot <- renderPlot({
      shape_data <- reconstructed_shape()
      req(shape_data)
      
      coords <- shape_data$coords
      
      # Create plot
      plot(coords, type = "l", lwd = 2, col = "steelblue", 
           asp = 1, xlab = "", ylab = "", main = "Reconstructed Shape",
           axes = FALSE, frame.plot = TRUE)
      polygon(coords, col = rgb(0.25, 0.55, 0.75, 0.3), border = "steelblue", lwd = 2)
      
      # Add PC scores as subtitle
      pc_text <- paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 2), collapse = ", ")
      mtext(pc_text, side = 3, line = 0.5, cex = 0.8, col = "gray30")
      
    }, height = function() input$plot_height)
    
    # Display reconstruction info
    output$reconstruction_info <- renderText({
      shape_data <- reconstructed_shape()
      req(shape_data)
      
      coords <- shape_data$coords
      paste0(
        "Reconstruction successful\n",
        "Number of outline points: ", nrow(coords), "\n",
        "X range: [", round(min(coords[,1]), 2), ", ", round(max(coords[,1]), 2), "]\n",
        "Y range: [", round(min(coords[,2]), 2), ", ", round(max(coords[,2]), 2), "]\n",
        "\nPC Scores used:\n",
        paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 3), collapse = "\n")
      )
    })
    
    # Download coordinates
    output$download_coords <- downloadHandler(
      filename = function() {
        paste0("reconstructed_shape_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        shape_data <- reconstructed_shape()
        req(shape_data)
        
        coords <- as.data.frame(shape_data$coords)
        colnames(coords) <- c("x", "y")
        coords$point <- 1:nrow(coords)
        
        # Add PC scores as header comments
        header_lines <- paste0("# ", names(shape_data$pc_scores), " = ", shape_data$pc_scores)
        
        writeLines(c(header_lines, ""), file)
        write.csv(coords[, c("point", "x", "y")], file, row.names = FALSE, append = TRUE)
      }
    )
    
    # Download plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("reconstructed_shape_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        shape_data <- reconstructed_shape()
        req(shape_data)
        
        coords <- shape_data$coords
        
        png(file, width = 800, height = 800, res = 150)
        plot(coords, type = "l", lwd = 2, col = "steelblue", 
             asp = 1, xlab = "", ylab = "", main = "Reconstructed Shape",
             axes = FALSE, frame.plot = TRUE)
        polygon(coords, col = rgb(0.25, 0.55, 0.75, 0.3), border = "steelblue", lwd = 2)
        pc_text <- paste(names(shape_data$pc_scores), "=", round(shape_data$pc_scores, 2), collapse = ", ")
        mtext(pc_text, side = 3, line = 0.5, cex = 0.8, col = "gray30")
        dev.off()
      }
    )
    
    # Batch reconstruction
    observeEvent(input$batch_reconstruct, {
      model <- loaded_model()
      
      if (is.null(model)) {
        showNotification("Please load a reconstruction model first.", type = "warning")
        return()
      }
      
      pc_axis <- as.integer(input$batch_pc)
      min_val <- input$batch_min
      max_val <- input$batch_max
      n_steps <- input$batch_steps
      
      withProgress(message = "Generating batch reconstruction...", value = 0, {
        # Generate PC score combinations
        pc_values <- seq(min_val, max_val, length.out = n_steps)
        
        batch_results <- list()
        
        for (i in seq_along(pc_values)) {
          incProgress(1 / n_steps, detail = sprintf("Shape %d/%d", i, n_steps))
          
          # Create PC score vector
          n_pcs <- min(10, model$parameters$n_components)
          pc_scores <- rep(0, n_pcs)
          
          if (!input$batch_hold_others) {
            # Use current UI values for other PCs
            pc_scores <- sapply(1:n_pcs, function(j) {
              input[[paste0("pc", j)]] %||% 0
            })
          }
          
          # Set the varying PC
          pc_scores[pc_axis] <- pc_values[i]
          names(pc_scores) <- paste0("PC", 1:n_pcs)
          
          # Reconstruct
          coords <- tryCatch({
            .reconstruct_single_shape(model, pc_scores)
          }, error = function(e) NULL)
          
          if (!is.null(coords)) {
            batch_results[[i]] <- list(coords = coords, pc_score = pc_values[i])
          }
        }
        
        batch_shapes(list(results = batch_results, pc_axis = pc_axis))
        showNotification(paste("Generated", length(batch_results), "shapes"), type = "message")
      })
    })
    
    # Plot batch results
    output$batch_plot <- renderPlot({
      batch_data <- batch_shapes()
      req(batch_data)
      
      results <- batch_data$results
      pc_axis <- batch_data$pc_axis
      n_shapes <- length(results)
      
      # Calculate grid dimensions
      ncols <- ceiling(sqrt(n_shapes))
      nrows <- ceiling(n_shapes / ncols)
      
      par(mfrow = c(nrows, ncols), mar = c(2, 2, 2, 1))
      
      for (i in seq_along(results)) {
        coords <- results[[i]]$coords
        pc_val <- results[[i]]$pc_score
        
        plot(coords, type = "l", lwd = 1.5, col = "steelblue", 
             asp = 1, xlab = "", ylab = "", 
             main = sprintf("PC%d = %.2f", pc_axis, pc_val),
             axes = FALSE, frame.plot = TRUE, cex.main = 0.9)
        polygon(coords, col = rgb(0.25, 0.55, 0.75, 0.2), border = "steelblue", lwd = 1.5)
      }
    })
    
    # Download batch grid
    output$download_batch <- downloadHandler(
      filename = function() {
        paste0("batch_reconstruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        batch_data <- batch_shapes()
        req(batch_data)
        
        results <- batch_data$results
        pc_axis <- batch_data$pc_axis
        n_shapes <- length(results)
        
        ncols <- ceiling(sqrt(n_shapes))
        nrows <- ceiling(n_shapes / ncols)
        
        png(file, width = 1600, height = 1600, res = 150)
        par(mfrow = c(nrows, ncols), mar = c(2, 2, 2, 1))
        
        for (i in seq_along(results)) {
          coords <- results[[i]]$coords
          pc_val <- results[[i]]$pc_score
          
          plot(coords, type = "l", lwd = 1.5, col = "steelblue", 
               asp = 1, xlab = "", ylab = "", 
               main = sprintf("PC%d = %.2f", pc_axis, pc_val),
               axes = FALSE, frame.plot = TRUE, cex.main = 0.9)
          polygon(coords, col = rgb(0.25, 0.55, 0.75, 0.2), border = "steelblue", lwd = 1.5)
        }
        dev.off()
      }
    )
    
    invisible(list(
      model = loaded_model,
      shape = reconstructed_shape,
      batch = batch_shapes
    ))
  })
}
