#' Data Import Module (Excel)
#'
#' UI and server logic to upload an Excel file (.xlsx/.xls), select a sheet,
#' and preview the data in a table.
#'
#' @param id Module id
#' @return Server returns a list with a reactive `data` containing the imported dataset.
#' @examples
#' # In UI: data_import_ui("import")
#' # In server: imported <- data_import_server("import")
#' @name data_import
NULL

#' @rdname data_import
#' @export
data_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Upload Excel File",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          fileInput(
            inputId = ns("file"),
            label = "Choose an Excel file (.xlsx or .xls)",
            accept = c(".xlsx", ".xls"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          uiOutput(ns("sheet_ui")),
          sliderInput(
            inputId = ns("n_rows"),
            label = "Preview rows",
            min = 5, max = 100, value = 20, step = 5
          )
        ),
        box(
          title = "Map shapes to data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          uiOutput(ns("id_col_ui")),
          uiOutput(ns("shape_dir_ui")),
          textOutput(ns("shape_dir_txt")),
          div(style = "margin: 6px 0;",
              actionButton(ns("map_shapes_btn"), "Map shapes", class = "btn-primary")
          ),
          br(),
          verbatimTextOutput(ns("mapping_summary"))
        ),
        box(
          title = "Data Preview",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          fluidRow(
            column(12,
              checkboxInput(ns("enable_edit"), "Enable editing", value = FALSE)
            )
          ),
          fluidRow(
            column(4, textInput(ns("new_col_name"), "New column name", value = "")),
            column(3, selectInput(ns("new_col_type"), "New column type", choices = c("character","numeric","logical"), selected = "character")),
            column(3, uiOutput(ns("new_col_default_ui"))),
            column(2, div(style = "margin-top: 24px;", actionButton(ns("add_col_btn"), "Add column", class = "btn-primary")))
          ),
          hr(),
          helpText("To overwrite the original Excel file, pick its location and confirm overwrite."),
          fluidRow(
            column(6,
              uiOutput(ns("save_excel_ui"))
            ),
            column(6, div(style = "margin-top: 28px;",
              textOutput(ns("save_status"))
            ))
          ),
          br(),
          DT::DTOutput(ns("preview")),
          br()
        )
      )
    )
  )
}

#' @rdname data_import
#' @export
data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: null-or-empty coalesce
    `%||%` <- function(a, b) {
      if (is.null(a)) return(b)
      if (is.character(a) && identical(length(a), 1L) && !nzchar(a)) return(b)
      a
    }

    # Working copy of data (editable)
    working_data <- reactiveVal(NULL)

    # Track selected sheet options
    sheet_names <- reactiveVal(character())

    # When a file is uploaded, list its sheets
    observeEvent(input$file, {
      req(input$file)
      path <- input$file$datapath
      # Store the source filename (for default save name)
      source_name <- input$file$name

      # Validate file extension
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "xls"), "Please upload a .xlsx or .xls file."))

      # Get sheet names safely
      sheets <- tryCatch({
        openxlsx::getSheetNames(path)
      }, error = function(e) {
        warning(e)
        character()
      })

      sheet_names(sheets)

      # Render a sheet selector if multiple sheets exist
      output$sheet_ui <- renderUI({
        if (length(sheets) == 0) {
          div(class = "text-danger", "No sheets found in the uploaded file.")
        } else if (length(sheets) == 1) {
          # Only one sheet: show its name; no input needed because we infer it
          tagList(p(strong("Sheet:"), sheets[[1]]))
        } else {
          selectInput(ns("sheet"), "Select sheet", choices = sheets, selected = sheets[[1]])
        }
      })
    }, ignoreInit = TRUE)

    # Reactive: read the data when file or sheet changes
    data_reactive <- reactive({
      req(input$file)
      sheets <- sheet_names()
      sheet_to_read <- if (length(sheets) == 1) sheets[[1]] else input$sheet
      req(sheet_to_read)

      # Read using openxlsx; detect dates and keep as data.frame
      df <- tryCatch({
        openxlsx::read.xlsx(
          xlsxFile = input$file$datapath,
          sheet = sheet_to_read,
          detectDates = TRUE
        )
      }, error = function(e) {
        validate(need(FALSE, paste("Failed to read sheet:", conditionMessage(e))))
        NULL
      })

      # Ensure it's a data.frame
      if (!is.null(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
      df
    })

    # Keep working_data synced with source data or mapping results
    observeEvent(data_reactive(), {
      df <- data_reactive()
      working_data(df)
    })

    # Dynamic ID column selector once data is available
    output$id_col_ui <- renderUI({
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      selectInput(ns("id_col"), "ID column for shape filenames", choices = names(df), selected = names(df)[1])
    })

    # shinyFiles directory picker for shape folder
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })
    output$shape_dir_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Directory picker unavailable"))
      shinyFiles::shinyDirButton(ns("shape_dir"), label = "Choose shape folder", title = "Select shape folder")
    })
    shape_dir_path <- reactiveVal("")
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
      shinyFiles::shinyDirChoose(input, id = "shape_dir", roots = roots, session = session)
    })
    observeEvent(input$shape_dir, {
      req(shinyfiles_ready())
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      sel <- try(shinyFiles::parseDirPath(roots, input$shape_dir), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) shape_dir_path(as.character(sel))
    })
    output$shape_dir_txt <- renderText({
      p <- shape_dir_path()
      if (is.null(p) || !nzchar(p)) "No folder selected" else p
    })

    # Save edited data back to Excel (user chooses target path)
    output$save_excel_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Save picker unavailable"))
      shinyFiles::shinySaveButton(ns("save_excel_btn"), label = "Save edited Excel", title = "Choose or overwrite .xlsx", filetype = list(xlsx = "xlsx"))
    })
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
      # Configure save dialog; default filename from uploaded file name if present
      default_file <- {
        nm <- input$file$name
        if (isTruthy(nm)) {
          base <- sub("\\\\.[^.]*$", "", nm)
          paste0(base, ".xlsx")
        } else {
          "edited_data.xlsx"
        }
      }
      shinyFiles::shinyFileSave(input, id = "save_excel_btn", roots = roots, session = session, defaultFile = default_file)
    })
    observeEvent(input$save_excel_btn, {
      if (!isTRUE(shinyfiles_ready())) return()
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      save_path_df <- try(shinyFiles::parseSavePath(roots, input$save_excel_btn), silent = TRUE)
      if (inherits(save_path_df, "try-error") || is.null(save_path_df) || nrow(save_path_df) == 0) {
        output$save_status <- renderText("Save cancelled or invalid path."); return()
      }
      path <- as.character(save_path_df$datapath[1])
      if (!grepl("\\.xlsx$", path, ignore.case = TRUE)) path <- paste0(path, ".xlsx")

      # Prepare data for Excel (drop list columns like 'shape')
      df <- working_data(); if (is.null(df)) df <- effective_data(); req(df)
      out <- df
      if ("shape" %in% names(out) && is.list(out$shape)) out$shape <- NULL

      # Ensure openxlsx is available
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        output$save_status <- renderText("Package 'openxlsx' is required. Please install it."); return()
      }
      # Attempt overwrite
      ok <- try({
        if (file.exists(path)) {
          # Best-effort overwrite handling
          try(unlink(path), silent = TRUE)
        }
        openxlsx::write.xlsx(out, path)
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        output$save_status <- renderText(paste0("Failed to save: ", conditionMessage(attr(ok, "condition"))))
      } else {
        output$save_status <- renderText(paste0("Saved: ", path))
      }
    })

    # Hold mapped data and summary
    mapped_data <- reactiveVal(NULL)
    mapping_summary <- reactiveVal(NULL)

    observeEvent(input$map_shapes_btn, {
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      id_col <- input$id_col; req(id_col)
      shp_dir <- shape_dir_path(); req(nzchar(shp_dir))

      # Ensure Momocs is available
      if (!requireNamespace("Momocs", quietly = TRUE)) {
        try(install.packages("Momocs", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
      }
      validate(need(requireNamespace("Momocs", quietly = TRUE), "Package 'Momocs' is required and could not be installed automatically."))

      # Fixed options (simple defaults); can be adjusted here if needed
      opts <- list(
        recursive = FALSE,
        case_sensitive = FALSE,
        fail_on_missing = FALSE,
        validate_shapes = TRUE
      )

      result <- tryCatch({
        map_shapes_to_data(
          data = df,
          id_col = id_col,
          shape_folder = shp_dir,
          shape_col = "shape",
          options = opts,
          verbose = TRUE
        )
      }, error = function(e) {
        showNotification(paste("Shape mapping failed:", conditionMessage(e)), type = "error")
        NULL
      })

      if (!is.null(result)) {
        mapped_data(result)
        mapping_summary(attr(result, "mapping_summary"))
        # Update working_data with mapped result (includes 'shape' column)
        working_data(result)
        showNotification("Shape mapping completed", type = "message")
      }
    })

    output$mapping_summary <- renderText({
      s <- mapping_summary(); if (is.null(s)) return("")
      paste0(
        "Rows: ", s$total_rows,
        " | Files found: ", s$files_found,
        " | Successful: ", s$successful_mappings,
        " | Failed imports: ", s$failed_imports,
        " | Missing: ", s$missing_shapes,
        " | Rate: ", s$mapping_rate, "%"
      )
    })

    # Effective data to use downstream: prefer mapped data when available
    effective_data <- reactive({
      df <- working_data()
      if (!is.null(df)) return(df)
      md <- mapped_data()
      if (!is.null(md)) return(md)
      data_reactive()
    })

    # Render preview table
    output$preview <- DT::renderDT({
      df <- working_data(); if (is.null(df)) df <- effective_data(); req(nrow(df) > 0)
      # Display-friendly copy for list columns like 'shape'
      display_df <- df
      if ("shape" %in% names(display_df) && is.list(display_df[["shape"]])) {
        display_df$shape <- vapply(display_df$shape, function(x) if (is.null(x)) "NULL" else "Out", character(1))
      }

      # Disable editing for problematic columns
      non_editable <- which(names(display_df) %in% c("shape"))
      # DT editable toggle
      edit_cfg <- if (isTRUE(input$enable_edit)) list(target = "cell", disable = list(columns = non_editable)) else FALSE

      # When editing enabled, disable ordering/filter to keep row indices stable
      page_len <- if (!is.null(input$n_rows)) input$n_rows else 20
      DT::datatable(
        display_df,
        options = list(pageLength = min(50, page_len), scrollX = TRUE, ordering = !isTRUE(input$enable_edit)),
        rownames = FALSE,
        filter = if (isTRUE(input$enable_edit)) "none" else "top",
        selection = "none",
        editable = edit_cfg
      )
    })

    # Apply cell edits to working_data
    observeEvent(input$preview_cell_edit, {
      if (!isTRUE(input$enable_edit)) return()
      info <- input$preview_cell_edit
      isolate({
        df <- working_data(); if (is.null(df)) return()
        r <- as.integer(info$row); j <- as.integer(info$col)
        # Guard
        if (j < 1 || j > ncol(df) || r < 1 || r > nrow(df)) return()
        col_name <- names(df)[j]
        # Skip non-editable
        if (col_name %in% c("shape")) return()
        val <- info$value
        # Coerce to column type
        if (is.numeric(df[[j]])) {
          new_val <- suppressWarnings(as.numeric(val))
        } else if (is.logical(df[[j]])) {
          v <- tolower(trimws(as.character(val)))
          new_val <- ifelse(v %in% c("true","t","1","yes"), TRUE,
                     ifelse(v %in% c("false","f","0","no"), FALSE, NA))
        } else {
          new_val <- as.character(val)
        }
        df[r, j] <- new_val
        working_data(df)
      })
    })


    # Export removed: edits apply in-place to working_data and propagate downstream
    # New column default UI based on type
    output$new_col_default_ui <- renderUI({
      type <- input$new_col_type %||% "character"
      if (identical(type, "numeric")) {
        numericInput(ns("new_col_default"), "Default", value = 0)
      } else if (identical(type, "logical")) {
        checkboxInput(ns("new_col_default"), "Default", value = FALSE)
      } else {
        textInput(ns("new_col_default"), "Default", value = "")
      }
    })

    # Add new column
    observeEvent(input$add_col_btn, {
      name <- input$new_col_name
      type <- input$new_col_type
      if (is.null(name) || !nzchar(name)) {
        showNotification("Please provide a column name.", type = "warning"); return()
      }
      df <- working_data(); if (is.null(df)) df <- data_reactive(); req(df)
      if (name %in% names(df)) {
        showNotification("A column with that name already exists.", type = "error"); return()
      }
      n <- nrow(df)
      # Fetch default value
      def <- input$new_col_default
      if (identical(type, "numeric")) {
        def <- suppressWarnings(as.numeric(def)); if (is.na(def)) def <- NA_real_
        df[[name]] <- rep(def, n)
      } else if (identical(type, "logical")) {
        def <- isTRUE(def)
        df[[name]] <- rep(def, n)
      } else {
        df[[name]] <- rep(as.character(def %||% ""), n)
      }
      working_data(df)
      showNotification(paste0("Column '", name, "' added."), type = "message")
    })

    # Types preview removed as requested

    # Return a list of reactives for downstream use
    return(list(
      data = effective_data
    ))
  })
}
