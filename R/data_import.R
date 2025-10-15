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
NULL

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
          DT::DTOutput(ns("preview")),
          br()
        )
      )
    )
  )
}

#' @export
data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track selected sheet options
    sheet_names <- reactiveVal(character())

    # When a file is uploaded, list its sheets
    observeEvent(input$file, {
      req(input$file)
      path <- input$file$datapath

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

    # Dynamic ID column selector once data is available
    output$id_col_ui <- renderUI({
      df <- data_reactive(); req(df)
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

    # Hold mapped data and summary
    mapped_data <- reactiveVal(NULL)
    mapping_summary <- reactiveVal(NULL)

    observeEvent(input$map_shapes_btn, {
      df <- data_reactive(); req(df)
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
      md <- mapped_data()
      if (!is.null(md)) return(md)
      data_reactive()
    })

    # Render preview table
    output$preview <- DT::renderDT({
      df <- effective_data()
      req(nrow(df) > 0)
      n <- if (!is.null(input$n_rows)) input$n_rows else 20
      # Make list-based shape columns preview-friendly
      if ("shape" %in% names(df) && is.list(df[["shape"]])) {
        df$shape <- vapply(df$shape, function(x) if (is.null(x)) "NULL" else "Out", character(1))
      }
      DT::datatable(
        head(df, n),
        options = list(pageLength = min(10, n), scrollX = TRUE),
        rownames = FALSE,
        filter = "top",
        selection = "none"
      )
    })

    # Types preview removed as requested

    # Return a list of reactives for downstream use
    return(list(
      data = effective_data
    ))
  })
}
