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
          numericInput(ns("plot_size"), "Plot size (pixels)", value = 600, min = 300, max = 1200, step = 50),
          
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
      
      # Collect PC scores from inputs
      n_pcs <- min(10, model$parameters$n_components)
      pc_scores <- sapply(1:n_pcs, function(i) {
        val <- input[[paste0("pc", i)]]
        if (is.null(val)) return(0)
        as.numeric(val)
      })
      names(pc_scores) <- paste0("PC", 1:n_pcs)
      
      # Debug: check collected scores
      if (any(is.na(pc_scores))) {
        showNotification("Invalid PC scores detected (NA values). Using 0 for missing values.", 
                        type = "warning", duration = 5)
        pc_scores[is.na(pc_scores)] <- 0
      }
      
      withProgress(message = "Reconstructing shape...", value = 0.5, {
        shape <- tryCatch({
          .reconstruct_single_shape(model, pc_scores)
        }, error = function(e) {
          # More detailed error message
          err_msg <- conditionMessage(e)
          showNotification(
            paste0("Reconstruction failed: ", err_msg, 
                   "\nModel dimensions: center=", length(model$center), 
                   ", rotation=", paste(dim(model$rotation), collapse="x")),
            type = "error", 
            duration = 10
          )
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
      # Ensure pc_scores is numeric vector
      pc_scores <- as.numeric(pc_scores)
      
      # Get dimensions
      n_coefs <- length(model$center)
      n_pcs <- ncol(model$rotation)
      
      # Debug: Print model structure details
      message("=== Model Structure Debug ===")
      message("Model components: ", paste(names(model), collapse = ", "))
      if (!is.null(model$parameters)) {
        message("Parameters: norm=", model$parameters$norm, 
                ", harmonics=", model$parameters$harmonics,
                ", n_harmonics_used=", model$parameters$n_harmonics_used)
      }
      message("EFA norm flag: ", if (!is.null(model$efa_norm)) model$efa_norm else "NULL")
      message("EFA method: ", if (!is.null(model$efa_method)) model$efa_method else "NULL")
      
      # Sample some original coefficients for comparison
      if (!is.null(model$efa_coe) && is.matrix(model$efa_coe)) {
        message("Original EFA coefficients (first specimen, first 8): ")
        message("  ", paste(round(model$efa_coe[1, 1:min(8, ncol(model$efa_coe))], 4), collapse = ", "))
        if (nrow(model$efa_coe) > 1) {
          message("Original EFA coefficients (mean of all specimens, first 8): ")
          coef_means <- colMeans(model$efa_coe, na.rm = TRUE)
          message("  ", paste(round(coef_means[1:min(8, length(coef_means))], 4), collapse = ", "))
        }
      }
      
      # Ensure pc_scores has correct length and pad with zeros if needed
      if (length(pc_scores) > n_pcs) {
        pc_scores <- pc_scores[1:n_pcs]
      } else if (length(pc_scores) < n_pcs) {
        full_scores <- rep(0, n_pcs)
        full_scores[1:length(pc_scores)] <- pc_scores
        pc_scores <- full_scores
      }
      
      # Reconstruct Fourier coefficients
      # PCA formula: X_reconstructed = center + scores %*% t(rotation)
      # But scores are typically in SD units, so we need to scale them
      # Standard PCA reconstruction: X = center + (scores * sdev) %*% t(rotation)
      # rotation matrix has eigenvectors as COLUMNS [n_coefs x n_pcs]
      # pc_scores is [n_pcs] in SD units
      # We need: center [n_coefs] + (pc_scores [n_pcs] * sdev [n_pcs]) %*% t(rotation [n_coefs x n_pcs])
      
      # Debug output
      message("=== Shape Reconstruction Debug ===")
      message("Input PC scores: ", paste(round(pc_scores, 3), collapse = ", "))
      message("Number of PCs: ", n_pcs)
      message("Number of coefficients: ", n_coefs)
      message("Center length: ", length(model$center))
      message("Rotation dims: ", paste(dim(model$rotation), collapse = " x "))
      message("Sdev length: ", length(model$sdev))
      
      # Scale PC scores by standard deviations
      scaled_scores <- pc_scores * model$sdev[1:length(pc_scores)]
      message("Scaled scores: ", paste(round(scaled_scores, 3), collapse = ", "))
      
      # Transform back to coefficient space
      # t(rotation) is [n_pcs x n_coefs], scaled_scores is [n_pcs]
      # Result: [n_coefs]
      contribution <- as.vector(scaled_scores %*% t(model$rotation))
      message("Contribution range: [", round(min(contribution), 3), ", ", round(max(contribution), 3), "]")
      
      reconstructed_coefs <- model$center + contribution
      message("Reconstructed coefs range: [", round(min(reconstructed_coefs), 3), ", ", round(max(reconstructed_coefs), 3), "]")
      message("Center range: [", round(min(model$center), 3), ", ", round(max(model$center), 3), "]")
      
      # Verify coefficient length
      if (length(reconstructed_coefs) != n_coefs) {
        stop("Coefficient length mismatch: ", length(reconstructed_coefs), " vs ", n_coefs)
      }
      
      # SIMPLIFIED APPROACH: Use the complete EFA object from the model
      # This preserves all Momocs internal structure and attributes
      if (!is.null(model$efa_object)) {
        message("Using complete EFA object for reconstruction")
        
        # Create a copy of the first specimen's EFA object structure
        reconstructed_coe <- model$efa_object
        
        # Replace ONLY the coefficients with our reconstructed values
        # Keep everything else (normalization params, class, attributes, etc.)
        reconstructed_coe$coe <- matrix(reconstructed_coefs, nrow = 1)
        rownames(reconstructed_coe$coe) <- "reconstructed"
        
        # Determine number of harmonics
        n_harmonics <- ncol(reconstructed_coe$coe) / 4
        message("Number of harmonics: ", n_harmonics)
        
        # Use Momocs efourier_i - it will handle all denormalization automatically
        message("Attempting Momocs efourier_i with complete object...")
        outline <- tryCatch({
          result <- efourier_i(reconstructed_coe, nb.h = as.integer(n_harmonics), nb.pts = 120)
          message("Momocs efourier_i succeeded!")
          result
        }, error = function(e) {
          message("Momocs efourier_i failed: ", conditionMessage(e))
          NULL
        })
        
        # Extract coordinates
        if (!is.null(outline)) {
          if (inherits(outline, "Out") && !is.null(outline$coo)) {
            coords <- outline$coo[[1]]
            message("Extracted coords from Out object: ", nrow(coords), " x ", ncol(coords))
          } else if (is.matrix(outline)) {
            coords <- outline
            message("Using outline as matrix directly: ", nrow(coords), " x ", ncol(coords))
          } else if (is.list(outline) && !is.null(outline[[1]])) {
            coords <- outline[[1]]
            message("Extracted coords from list: ", nrow(coords), " x ", ncol(coords))
          } else {
            message("Outline format not recognized")
            outline <- NULL
          }
        }
        
      } else {
        # Fallback: build OutCoe manually (old approach)
        message("EFA object not in model, using manual OutCoe construction")
        outline <- NULL  # Will trigger manual reconstruction below
      }
      
      # Fallback: manual reconstruction if Momocs failed
      if (is.null(outline)) {
        message("Using manual inverse Fourier transformation")
        nb_pts <- 120
        theta <- seq(0, 2 * pi, length.out = nb_pts + 1)[-(nb_pts + 1)]
        x <- numeric(nb_pts)
        y <- numeric(nb_pts)
        
        # Standard inverse Fourier: sum harmonics
        for (h in 1:n_harmonics) {
          idx <- (h - 1) * 4 + 1:4
          An <- reconstructed_coefs[idx[1]]
          Bn <- reconstructed_coefs[idx[2]]
          Cn <- reconstructed_coefs[idx[3]]
          Dn <- reconstructed_coefs[idx[4]]
          
          x <- x + An * cos(h * theta) + Bn * sin(h * theta)
          y <- y + Cn * cos(h * theta) + Dn * sin(h * theta)
        }
        
        # Apply denormalization if parameters are available
        if (!is.null(reconstructed_coe$norm) && reconstructed_coe$norm == TRUE) {
          message("Applying denormalization transformations...")
          
          # Try to use shape_metadata (our manual capture) first
          if (!is.null(model$shape_metadata) && length(model$shape_metadata) > 0) {
            # Calculate mean size and centroid from all shapes
            sizes <- sapply(model$shape_metadata, function(m) m$size)
            centroids <- t(sapply(model$shape_metadata, function(m) m$centroid))
            
            mean_size <- mean(sizes, na.rm = TRUE)
            mean_centroid <- colMeans(centroids, na.rm = TRUE)
            
            message("  Using shape metadata for denormalization")
            message("  Mean size: ", round(mean_size, 3))
            message("  Mean centroid: [", paste(round(mean_centroid, 3), collapse = ", "), "]")
            
            # Scale by mean size
            if (mean_size > 0) {
              x <- x * mean_size
              y <- y * mean_size
            }
            
            # Translate by mean centroid
            x <- x + mean_centroid[1]
            y <- y + mean_centroid[2]
            
          } else {
            # Fallback to r1/r2/baseline if available
            r1 <- reconstructed_coe$r1
            r2 <- reconstructed_coe$r2
            baseline1 <- reconstructed_coe$baseline1
            baseline2 <- reconstructed_coe$baseline2
            
            if (!is.null(r1) && !is.null(r2) && !is.null(baseline1) && !is.null(baseline2)) {
              # Scale by size (r2 is the size parameter)
              if (r2 > 0) {
                x <- x * r2
                y <- y * r2
                message("  Scaled by r2 = ", round(r2, 3))
              }
              
              # Translate by baseline (centroid position)
              if (length(baseline1) >= 2) {
                x <- x + baseline1[1]
                y <- y + baseline1[2]
                message("  Translated by baseline1 = [", paste(round(baseline1, 3), collapse = ", "), "]")
              }
            } else {
              message("  Warning: No denormalization parameters available")
            }
          }
        }
        
        coords <- cbind(x, y)
        message("Manual reconstruction complete: ", nrow(coords), " points")
        message("X range: [", round(min(x), 3), ", ", round(max(x), 3), "]")
        message("Y range: [", round(min(y), 3), ", ", round(max(y), 3), "]")
        
        # Check if shape looks degenerate
        x_range <- max(x) - min(x)
        y_range <- max(y) - min(y)
        aspect_ratio <- if (y_range > 0) x_range / y_range else 0
        
        message("Aspect ratio X/Y: ", round(aspect_ratio, 3))
        
        if (x_range < 0.01 && y_range < 0.01) {
          warning("Reconstructed shape is very small. This may indicate an issue with the coefficients.")
        }
        if (aspect_ratio < 0.1 || aspect_ratio > 10) {
          warning("Reconstructed shape has extreme aspect ratio: ", round(aspect_ratio, 3), 
                  ". This may indicate missing or incorrect normalization parameters.")
        }
      }
      
      message("=== Reconstruction Complete ===\n")
      
      # Ensure coords is a matrix with 2 columns
      if (!is.matrix(coords) || ncol(coords) != 2) {
        stop("Reconstructed coordinates are not a valid 2-column matrix")
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
      
    }, height = function() {
      size <- input$plot_size
      if (is.null(size) || !is.numeric(size)) return(600)
      return(as.integer(size))
    })
    
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
