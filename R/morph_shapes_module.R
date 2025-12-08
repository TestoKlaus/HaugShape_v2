#' Morph Shapes Module - Split and Morph Images
#'
#' UI and server logic for uploading images, splitting them at a specified position,
#' and morphing the resulting halves back together using advanced morphing algorithms.
#'
#' @param id Module id
#' @return Server returns a reactive containing processing results
#' @name morph_shapes_module
NULL

#' @rdname morph_shapes_module
#' @export
morph_shapes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          title = "1. Upload Image",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          fileInput(
            ns("upload_image"),
            "Choose an image file",
            accept = c("image/png", "image/jpeg", "image/jpg", "image/tiff", "image/bmp"),
            multiple = FALSE
          ),
          
          conditionalPanel(
            condition = sprintf("output['%s']", ns("image_uploaded")),
            box(
              title = "Image Preview",
              status = "info",
              width = 12,
              plotOutput(ns("preview_plot"), height = "400px"),
              br(),
              verbatimTextOutput(ns("image_info"))
            )
          )
        )
      ),
      
      column(
        width = 6,
        box(
          title = "2. Split Configuration",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          
          selectInput(
            ns("split_direction"),
            "Split Direction",
            choices = c("Vertical (left/right)" = "vertical", 
                       "Horizontal (top/bottom)" = "horizontal"),
            selected = "vertical"
          ),
          
          sliderInput(
            ns("split_position"),
            "Split Position (0 = left/top, 1 = right/bottom)",
            min = 0.1,
            max = 0.9,
            value = 0.5,
            step = 0.01
          ),
          
          radioButtons(
            ns("mirror_parts"),
            "Mirror which parts?",
            choices = c("None" = "none",
                       "First part (left/top)" = "first",
                       "Second part (right/bottom)" = "second",
                       "Both parts" = "both"),
            selected = "second"
          ),
          
          hr(),
          
          uiOutput(ns("output_dir_ui")),
          
          textInput(
            ns("output_folder_name"),
            "Subfolder name for split images",
            value = format(Sys.time(), "split_%Y%m%d_%H%M%S")
          ),
          
          actionButton(
            ns("btn_split"),
            "Split Image",
            icon = icon("cut"),
            class = "btn-warning",
            style = "width: 100%;"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = sprintf("output['%s']", ns("split_completed")),
          box(
            title = "3. Split Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            fluidRow(
              column(
                width = 6,
                box(
                  title = "First Half",
                  width = 12,
                  status = "info",
                  plotOutput(ns("split_preview_first"), height = "300px"),
                  verbatimTextOutput(ns("split_path_first"))
                )
              ),
              column(
                width = 6,
                box(
                  title = "Second Half",
                  width = 12,
                  status = "info",
                  plotOutput(ns("split_preview_second"), height = "300px"),
                  verbatimTextOutput(ns("split_path_second"))
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = sprintf("output['%s']", ns("split_completed")),
          box(
            title = "4. Morphing Configuration",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            fluidRow(
              column(
                width = 4,
                selectInput(
                  ns("morph_method"),
                  "Morphing Method",
                  choices = c("Distance Transform" = "distance_transform",
                             "Linear" = "linear",
                             "Spline" = "spline"),
                  selected = "distance_transform"
                ),
                
                numericInput(
                  ns("n_steps"),
                  "Number of morphing steps",
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1
                )
              ),
              
              column(
                width = 4,
                numericInput(
                  ns("threshold"),
                  "Binary threshold (0-1)",
                  value = 0.1,
                  min = 0,
                  max = 1,
                  step = 0.05
                ),
                
                numericInput(
                  ns("gamma"),
                  "Gamma correction",
                  value = 1.0,
                  min = 0.1,
                  max = 3.0,
                  step = 0.1
                )
              ),
              
              column(
                width = 4,
                numericInput(
                  ns("blur_sigma"),
                  "Blur sigma (smoothing)",
                  value = 0,
                  min = 0,
                  max = 5,
                  step = 0.1
                ),
                
                checkboxInput(
                  ns("auto_align"),
                  "Auto-align shapes",
                  value = TRUE
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                textInput(
                  ns("morph_subfolder"),
                  "Subfolder name for morphed images",
                  value = format(Sys.time(), "morphed_%Y%m%d_%H%M%S")
                ),
                
                actionButton(
                  ns("btn_morph"),
                  "Generate Morphed Shapes",
                  icon = icon("wand-magic-sparkles"),
                  class = "btn-success",
                  style = "width: 100%;"
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = sprintf("output['%s']", ns("morph_completed")),
          box(
            title = "5. Morphing Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            verbatimTextOutput(ns("morph_summary")),
            
            hr(),
            
            sliderInput(
              ns("morph_preview_step"),
              "Preview morphing step:",
              min = 1,
              max = 5,
              value = 1,
              step = 1,
              animate = animationOptions(interval = 500)
            ),
            
            plotOutput(ns("morph_preview"), height = "500px")
          )
        )
      )
    )
  )
}

#' @rdname morph_shapes_module
#' @export
morph_shapes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store state
    rv <- reactiveValues(
      uploaded_image = NULL,
      uploaded_path = NULL,
      split_result = NULL,
      morph_result = NULL,
      split_paths = list(first = NULL, second = NULL),
      magick_ready = FALSE,
      imager_ready = FALSE,
      shinyfiles_ready = FALSE
    )
    
    # Check package availability
    observe({
      rv$magick_ready <- requireNamespace("magick", quietly = TRUE)
      rv$imager_ready <- requireNamespace("imager", quietly = TRUE)
      rv$shinyfiles_ready <- requireNamespace("shinyFiles", quietly = TRUE)
      
      # Try to install missing packages silently
      if (!rv$magick_ready) {
        try({
          install.packages("magick", repos = "https://cran.r-project.org", quiet = TRUE)
          rv$magick_ready <- requireNamespace("magick", quietly = TRUE)
        }, silent = TRUE)
      }
      
      if (!rv$imager_ready) {
        try({
          install.packages("imager", repos = "https://cran.r-project.org", quiet = TRUE)
          rv$imager_ready <- requireNamespace("imager", quietly = TRUE)
        }, silent = TRUE)
      }
    })
    
    # Output directory UI
    output$output_dir_ui <- renderUI({
      if (rv$shinyfiles_ready) {
        tagList(
          shinyFiles::shinyDirButton(
            ns("output_dir_btn"),
            label = "Choose output directory",
            title = "Select output directory"
          ),
          br(),
          strong("Output directory: "),
          textOutput(ns("output_dir_display"), inline = TRUE)
        )
      } else {
        textInput(
          ns("output_dir_fallback"),
          "Output directory",
          value = file.path(getwd(), "morph_output")
        )
      }
    })
    
    # Selected output directory
    selected_output_dir <- reactiveVal(file.path(getwd(), "morph_output"))
    
    # Setup directory chooser
    observeEvent(rv$shinyfiles_ready, ignoreInit = FALSE, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      shinyFiles::shinyDirChoose(input, id = "output_dir_btn", roots = vols, session = session)
    })
    
    observeEvent(input$output_dir_btn, {
      if (!rv$shinyfiles_ready) return()
      
      vols <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(vols, "try-error") || is.null(vols) || length(vols) == 0) {
        vols <- c(Home = normalizePath("~"))
      }
      
      sel <- try(shinyFiles::parseDirPath(vols, input$output_dir_btn), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) == 1) {
        selected_output_dir(as.character(sel))
      }
    })
    
    output$output_dir_display <- renderText({
      dir_path <- selected_output_dir()
      if (!is.null(dir_path) && nzchar(dir_path)) dir_path else "(none)"
    })
    
    # Handle image upload
    observeEvent(input$upload_image, {
      req(input$upload_image)
      
      if (!rv$magick_ready) {
        showNotification(
          "Package 'magick' is required. Please install it: install.packages('magick')",
          type = "error",
          duration = 10
        )
        return()
      }
      
      tryCatch({
        # Load image using magick
        img <- magick::image_read(input$upload_image$datapath)
        rv$uploaded_image <- img
        rv$uploaded_path <- input$upload_image$datapath
        
        showNotification("Image uploaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Error loading image:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Image uploaded flag
    output$image_uploaded <- reactive({
      !is.null(rv$uploaded_image)
    })
    outputOptions(output, "image_uploaded", suspendWhenHidden = FALSE)
    
    # Preview uploaded image
    output$preview_plot <- renderPlot({
      req(rv$uploaded_image)
      
      tryCatch({
        plot(rv$uploaded_image)
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error displaying image:", e$message))
      })
    })
    
    # Display image info
    output$image_info <- renderText({
      req(rv$uploaded_image)
      
      info <- magick::image_info(rv$uploaded_image)
      paste0(
        "Format: ", info$format, "\n",
        "Dimensions: ", info$width, " x ", info$height, " pixels\n",
        "Colorspace: ", info$colorspace, "\n",
        "Filesize: ", round(info$filesize / 1024, 2), " KB"
      )
    })
    
    # Split image
    observeEvent(input$btn_split, {
      req(rv$uploaded_image)
      
      if (!rv$magick_ready) {
        showNotification(
          "Package 'magick' is required. Please install it.",
          type = "error"
        )
        return()
      }
      
      # Get output directory
      output_dir <- if (rv$shinyfiles_ready) {
        selected_output_dir()
      } else {
        input$output_dir_fallback
      }
      
      if (is.null(output_dir) || !nzchar(output_dir)) {
        output_dir <- file.path(getwd(), "morph_output")
      }
      
      # Create split subfolder
      split_dir <- file.path(output_dir, input$output_folder_name)
      
      withProgress(message = "Splitting image...", value = 0, {
        tryCatch({
          # Prepare temporary file for the uploaded image
          temp_input <- tempfile(fileext = paste0(".", tools::file_ext(input$upload_image$name)))
          magick::image_write(rv$uploaded_image, temp_input)
          
          incProgress(0.3, detail = "Processing split...")
          
          # Call split_image function
          result <- split_image(
            input_paths = temp_input,
            output_dir = split_dir,
            split_options = list(
              direction = input$split_direction,
              split_position = input$split_position,
              mirror_parts = input$mirror_parts
            ),
            naming_options = list(
              suffix_first = if (input$split_direction == "vertical") "_left" else "_top",
              suffix_second = if (input$split_direction == "vertical") "_right" else "_bottom",
              prefix = "",
              numbering = FALSE
            ),
            processing_options = list(
              overwrite = TRUE,
              quality = 95,
              format = "auto",
              preserve_metadata = TRUE
            ),
            verbose = FALSE
          )
          
          incProgress(0.4, detail = "Loading split results...")
          
          # Store results
          rv$split_result <- result
          
          # Load split images for preview
          if (nrow(result$processed_files) >= 2) {
            rv$split_paths$first <- result$processed_files$output_path[1]
            rv$split_paths$second <- result$processed_files$output_path[2]
          }
          
          incProgress(0.3, detail = "Complete!")
          
          showNotification(
            paste("Image split successfully! Files saved to:", split_dir),
            type = "message",
            duration = 5
          )
          
          # Clean up temp file
          unlink(temp_input)
          
        }, error = function(e) {
          showNotification(
            paste("Error splitting image:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
    
    # Split completed flag
    output$split_completed <- reactive({
      !is.null(rv$split_result) && !is.null(rv$split_paths$first)
    })
    outputOptions(output, "split_completed", suspendWhenHidden = FALSE)
    
    # Preview split results
    output$split_preview_first <- renderPlot({
      req(rv$split_paths$first)
      req(file.exists(rv$split_paths$first))
      
      tryCatch({
        img <- magick::image_read(rv$split_paths$first)
        plot(img)
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:", e$message))
      })
    })
    
    output$split_preview_second <- renderPlot({
      req(rv$split_paths$second)
      req(file.exists(rv$split_paths$second))
      
      tryCatch({
        img <- magick::image_read(rv$split_paths$second)
        plot(img)
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error:", e$message))
      })
    })
    
    output$split_path_first <- renderText({
      req(rv$split_paths$first)
      paste("Saved to:\n", rv$split_paths$first)
    })
    
    output$split_path_second <- renderText({
      req(rv$split_paths$second)
      paste("Saved to:\n", rv$split_paths$second)
    })
    
    # Update morph step slider when morphing completes
    observe({
      req(rv$morph_result)
      req(!is.null(input$n_steps))
      
      updateSliderInput(
        session,
        "morph_preview_step",
        max = input$n_steps
      )
    })
    
    # Morph shapes
    observeEvent(input$btn_morph, {
      req(rv$split_paths$first, rv$split_paths$second)
      req(file.exists(rv$split_paths$first))
      req(file.exists(rv$split_paths$second))
      
      if (!rv$imager_ready) {
        showNotification(
          "Package 'imager' is required for morphing. Please install it: install.packages('imager')",
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Get output directory
      output_dir <- if (rv$shinyfiles_ready) {
        selected_output_dir()
      } else {
        input$output_dir_fallback
      }
      
      if (is.null(output_dir) || !nzchar(output_dir)) {
        output_dir <- file.path(getwd(), "morph_output")
      }
      
      # Create morph subfolder
      morph_dir <- file.path(output_dir, input$morph_subfolder)
      
      withProgress(message = "Morphing shapes...", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Preparing morphing...")
          
          # Call morph_shapes function
          result <- morph_shapes(
            input_paths = c(rv$split_paths$first, rv$split_paths$second),
            output_dir = morph_dir,
            morphing_options = list(
              method = input$morph_method,
              n_steps = input$n_steps,
              morph_type = "pair",
              interpolation = "linear"
            ),
            processing_options = list(
              threshold = input$threshold,
              gamma = input$gamma,
              blur_sigma = input$blur_sigma,
              auto_align = input$auto_align
            ),
            distance_options = list(
              distance_metric = "euclidean",
              normalize_distances = TRUE,
              invert_distances = FALSE
            ),
            blending_options = list(
              blend_mode = "average",
              weights = NULL,
              edge_enhancement = FALSE
            ),
            output_options = list(
              format = "png",
              naming_pattern = "morph_{step}",
              save_intermediates = FALSE
            ),
            validation_options = list(
              check_dimensions = TRUE,
              validate_binary = TRUE,
              similarity_threshold = 0
            ),
            export_options = list(),
            verbose = FALSE
          )
          
          incProgress(0.6, detail = "Loading results...")
          
          # Store results
          rv$morph_result <- result
          
          incProgress(0.2, detail = "Complete!")
          
          showNotification(
            paste("Morphing completed!", length(result$morphed_images), "images created in:", morph_dir),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          showNotification(
            paste("Error morphing shapes:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })
    
    # Morph completed flag
    output$morph_completed <- reactive({
      !is.null(rv$morph_result)
    })
    outputOptions(output, "morph_completed", suspendWhenHidden = FALSE)
    
    # Display morph summary
    output$morph_summary <- renderText({
      req(rv$morph_result)
      
      summary <- rv$morph_result$processing_summary
      paste0(
        "Morphing Summary\n",
        "================\n",
        "Total morphed images: ", summary$total_morphed_images, "\n",
        "Morphing method: ", summary$morphing_method, "\n",
        "Morph type: ", summary$morph_type, "\n",
        "Steps per pair: ", summary$n_steps_per_pair, "\n",
        "Output format: ", summary$output_format, "\n",
        "Alignment applied: ", summary$alignment_applied, "\n",
        "\nProcessing Options:\n",
        "  Threshold: ", summary$processing_options$threshold, "\n",
        "  Gamma: ", summary$processing_options$gamma, "\n",
        "  Blur sigma: ", summary$processing_options$blur_sigma
      )
    })
    
    # Preview morphed result
    output$morph_preview <- renderPlot({
      req(rv$morph_result)
      req(input$morph_preview_step)
      
      step <- input$morph_preview_step
      morphed_images <- rv$morph_result$morphed_images
      
      if (step > length(morphed_images)) {
        step <- length(morphed_images)
      }
      
      tryCatch({
        if (rv$imager_ready) {
          img <- imager::load.image(morphed_images[step])
          plot(img, axes = FALSE, main = paste("Morphing Step", step))
        } else {
          # Fallback to magick
          img <- magick::image_read(morphed_images[step])
          plot(img)
          title(paste("Morphing Step", step))
        }
      }, error = function(e) {
        plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Error displaying morphed image:", e$message))
      })
    })
    
    # Return reactive with results
    reactive({
      list(
        split_result = rv$split_result,
        morph_result = rv$morph_result,
        split_paths = rv$split_paths
      )
    })
  })
}
