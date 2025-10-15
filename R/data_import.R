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
          ),
          checkboxInput(
            inputId = ns("show_types"),
            label = "Show column types",
            value = FALSE
          )
        ),
        box(
          title = "Data Preview",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::DTOutput(ns("preview")),
          br(),
          uiOutput(ns("types_ui"))
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

    # Render preview table
    output$preview <- DT::renderDT({
      df <- data_reactive()
      req(nrow(df) > 0)
      n <- if (!is.null(input$n_rows)) input$n_rows else 20
      DT::datatable(
        head(df, n),
        options = list(pageLength = min(10, n), scrollX = TRUE),
        rownames = FALSE,
        filter = "top",
        selection = "none"
      )
    })

    # Show column types if requested
    output$types_ui <- renderUI({
      req(input$show_types)
      df <- data_reactive()
      req(df)
      types <- vapply(df, function(x) paste(class(x), collapse = ", "), character(1))
      tags$div(
        tags$h4("Column types"),
        tags$pre(paste(sprintf("%s: %s", names(types), types), collapse = "\n"))
      )
    })

    # Return a list of reactives for downstream use
    return(list(
      data = data_reactive
    ))
  })
}
