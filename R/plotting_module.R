#' Plotting Module (shape_plot)
#'
#' UI and server to configure and render plots using `shape_plot()`.
#' Exposes the main parameters and consumes data from the Data Import module.
#'
#' @param id Module id
#' @param data_reactive A reactive function returning a data.frame from Data Import
#' @export
plotting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        box(
          title = "Data mapping",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          selectInput(ns("x_col"), "X column", choices = NULL),
          selectInput(ns("y_col"), "Y column", choices = NULL),
          selectInput(ns("group_col"), "Group column (optional)", choices = c("(none)" = "")),
          uiOutput(ns("group_vals_ui"))
        ),
        box(
          title = "Styling",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          selectInput(ns("plot_style"), "Plot style", choices = c("Haug", "inverted_Haug", "publication"), selected = "Haug"),
          numericInput(ns("point_size"), "Point size", value = 2, min = 0.1, step = 0.1),
          uiOutput(ns("point_group_color_pickers")),
          uiOutput(ns("point_group_fill_pickers")),
          uiOutput(ns("point_group_shape_pickers")),
          numericInput(ns("title_size"), "Title size", value = 24, min = 6, step = 1),
          numericInput(ns("label_size"), "Axis label size", value = 20, min = 6, step = 1),
          numericInput(ns("tick_size"), "Tick label size", value = 15, min = 6, step = 1),
          numericInput(ns("axis_linewidth"), "Axis linewidth", value = 1, min = 0, step = 0.25),
          numericInput(ns("tick_length"), "Tick length (npc)", value = 0.005, min = 0, step = 0.001),
          numericInput(ns("tick_margin"), "Tick margin", value = 0.05, min = 0, step = 0.01)
        ),
        box(
          title = "Axis & Aspect",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          selectInput(
            ns("aspect"),
            "Plot aspect ratio",
            choices = c(
              "Auto (free unless shapes)" = "auto",
              "Free (no lock)" = "free",
              "1:1" = "1:1",
              "2:1" = "2:1"
            ),
            selected = "auto"
          )
        ),
        box(
          title = "Features - Hulls",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("hulls_show"), "Show hulls", value = FALSE),
          uiOutput(ns("hull_groups_ui")),
          # Per-group controls for hulls
          uiOutput(ns("hull_group_fill_pickers")),
          uiOutput(ns("hull_group_border_pickers")),
          uiOutput(ns("hull_group_alpha_inputs")),
          uiOutput(ns("hull_group_linetype_inputs")),
          uiOutput(ns("hull_group_linewidth_inputs"))
        ),
        box(
          title = "Features - Contours",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("contours_show"), "Show contours", value = FALSE),
          uiOutput(ns("contour_groups_ui")),
          uiOutput(ns("contour_group_color_pickers")),
          numericInput(ns("contour_linewidth"), "Contour linewidth", value = 0.5, min = 0, step = 0.1)
        ),
        box(
          title = "Features - Shape overlays",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("shapes_show"), "Overlay shapes (requires 'shape' column)", value = FALSE),
          checkboxInput(ns("shapes_only_hull"), "Only show shapes at convex hull points", value = TRUE),
          uiOutput(ns("shape_groups_ui")),
          textInput(ns("shape_col"), "Shape column name", value = "shape"),
          numericInput(ns("shape_size"), "Shape overlay size", value = 0.01, min = 0, step = 0.01),
          numericInput(ns("shape_shift"), "Shape overlay shift", value = 0.1, min = 0, step = 0.01),
          numericInput(ns("shape_x_adjust"), "Shape x adjust", value = 0, step = 0.01),
          numericInput(ns("shape_y_adjust"), "Shape y adjust", value = 0, step = 0.01)
        ),
        box(
          title = "Labels",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          textInput(ns("title"), "Plot title", value = NULL, placeholder = "Optional title"),
          textInput(ns("x_label"), "X axis label", value = NULL, placeholder = "Default: X column name"),
          textInput(ns("y_label"), "Y axis label", value = NULL, placeholder = "Default: Y column name"),
          textInput(ns("x_adjust"), "X label adjust (x,y)", value = "0,0"),
          textInput(ns("y_adjust"), "Y label adjust (x,y)", value = "0,0"),
          numericInput(ns("x_size"), "X label size", value = 5, min = 1, step = 0.5),
          numericInput(ns("y_size"), "Y label size", value = 5, min = 1, step = 0.5),
          checkboxInput(ns("rotate_y"), "Rotate Y label", value = FALSE),
          checkboxInput(ns("show_borders"), "Show borders", value = TRUE)
        ),
        box(
          title = "Export",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          checkboxInput(ns("export"), "Export using shape_plot", value = FALSE),
          textInput(ns("export_filename"), "Filename (without extension)", value = "shape_plot_output"),
          uiOutput(ns("export_dir_ui")),
          textOutput(ns("export_dir_txt")),
          selectInput(ns("export_format"), "Format", choices = c("tiff","jpg"), selected = "tiff"),
          checkboxInput(ns("export_custom_size"), "Set custom size (inches)", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("export_custom_size")),
            numericInput(ns("export_width"), "Width (inches)", value = 8, min = 1, step = 0.5),
            numericInput(ns("export_height"), "Height (inches)", value = 8, min = 1, step = 0.5)
          ),
          numericInput(ns("export_dpi"), "DPI", value = 300, min = 72, step = 10),
          tags$div(style = "margin-top:6px;", textOutput(ns("preview_size_txt"))),
          numericInput(ns("export_scale"), "Scale (× preview pixels)", value = 3, min = 1, step = 0.5),
          tags$hr(),
          helpText("Prefer to edit the plot later in RStudio? Download the ggplot object:"),
          downloadButton(ns("download_plot_rds"), "Download ggplot (.rds)")
        ),
        div(style = "margin: 10px 0;",
            actionButton(ns("render"), "Render plot", class = "btn-success btn-lg")
        ),
        box(
          title = "Plot",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = FALSE,
          plotOutput(ns("plot"), height = 600),
          br(),
          verbatimTextOutput(ns("messages"))
        )
      ),
      column(
        width = 12,
        box(
          title = "Legend / Info",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          helpText("Summary of what is currently displayed in the plot."),
          htmlOutput(ns("legend_html"))
        )
      )
    )
  )
}

#' @export
plotting_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: null-or-empty coalesce
    `%||%` <- function(a, b) {
      if (is.null(a)) return(b)
      if (is.character(a) && identical(length(a), 1L) && !nzchar(a)) return(b)
      a
    }

    # Ensure colourpicker is available (auto-install quietly like other modules)
    colourpicker_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("colourpicker", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("colourpicker", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("colourpicker", quietly = TRUE)
      }
      colourpicker_ready(isTRUE(ready))
    })

    # Directory picker for export using shinyFiles
    shinyfiles_ready <- reactiveVal(FALSE)
    observe({
      ready <- requireNamespace("shinyFiles", quietly = TRUE)
      if (!isTRUE(ready)) {
        try(install.packages("shinyFiles", repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
        ready <- requireNamespace("shinyFiles", quietly = TRUE)
      }
      shinyfiles_ready(isTRUE(ready))
    })

    # Dynamic per-group point color/fill/shape pickers
    output$point_group_color_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        colourpicker::colourInput(ns(paste0("point_color_", safe_id(g))), paste0("Point color: ", g), value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })
    output$point_group_fill_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        colourpicker::colourInput(ns(paste0("point_fill_", safe_id(g))), paste0("Point fill: ", g), value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })
    output$point_group_shape_pickers <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      groups <- if (!is.null(input$group_vals) && length(input$group_vals)) input$group_vals else unique(df[[gcol]])
      choices <- c(
        "Circle filled (21)" = 21,
        "Circle solid (16)" = 16,
        "Square filled (22)" = 22,
        "Diamond filled (23)" = 23,
        "Triangle up (24)" = 24,
        "Circle open (1)" = 1,
        "Square open (0)" = 0
      )
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        selectInput(ns(paste0("point_shape_", safe_id(g))), paste0("Point shape: ", g), choices = choices, selected = 21)
      })
      do.call(tagList, picker_list)
    })
    # Dynamic per-group hull fill color pickers
    output$hull_group_fill_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      # Default palette
      pal <- tryCatch({
        if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups))
      }, error = function(...) rep("#1f77b4", length(groups)))
      # Sanitize ID helper
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      # Build pickers
      picker_list <- mapply(function(g, default_col) {
        inputId <- ns(paste0("hull_fill_", safe_id(g)))
        label <- paste0("Hull fill color: ", g)
        colourpicker::colourInput(inputId, label, value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })
    output$hull_group_border_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        inputId <- ns(paste0("hull_border_", safe_id(g)))
        label <- paste0("Hull border color: ", g)
        colourpicker::colourInput(inputId, label, value = "black")
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull alpha inputs
    output$hull_group_alpha_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        numericInput(ns(paste0("hull_alpha_", safe_id(g))), paste0("Hull alpha: ", g), value = 0.1, min = 0, max = 1, step = 0.05)
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull linetype inputs
    output$hull_group_linetype_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      choices <- c("solid","dashed","dotted","dotdash","longdash","twodash")
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        selectInput(ns(paste0("hull_linetype_", safe_id(g))), paste0("Hull linetype: ", g), choices = choices, selected = "solid")
      })
      do.call(tagList, picker_list)
    })
    # Per-group hull linewidth inputs
    output$hull_group_linewidth_inputs <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$hulls_show)) return(NULL)
      groups <- input$hull_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        numericInput(ns(paste0("hull_linewidth_", safe_id(g))), paste0("Hull linewidth: ", g), value = 0.5, min = 0, step = 0.1)
      })
      do.call(tagList, picker_list)
    })
    # Dynamic per-group contour color pickers
    output$contour_group_color_pickers <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      if (!isTRUE(input$contours_show)) return(NULL)
      groups <- input$contour_groups
      if (is.null(groups) || length(groups) == 0) return(NULL)
      # Default to a palette like hulls
      pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(groups)) else rep("#1f77b4", length(groups)) }, error = function(...) rep("#1f77b4", length(groups)))
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- mapply(function(g, default_col) {
        inputId <- ns(paste0("contour_color_", safe_id(g)))
        label <- paste0("Contour color: ", g)
        colourpicker::colourInput(inputId, label, value = default_col)
      }, groups, pal, SIMPLIFY = FALSE)
      do.call(tagList, picker_list)
    })

    # Helper: parse comma-separated values into vector (legacy UI removed, keep in case of inputs from sessions)
    parse_csv <- function(x) {
      if (is.null(x) || !nzchar(x)) return(character())
      parts <- unlist(strsplit(x, ","))
      trimws(parts)
    }

    # Update column selectors when data changes
    observe({
      df <- data_reactive()
      if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return()
      cols <- names(df)
      updateSelectInput(session, "x_col", choices = cols, selected = if (!is.null(input$x_col) && input$x_col %in% cols) input$x_col else cols[1])
      updateSelectInput(session, "y_col", choices = cols, selected = if (!is.null(input$y_col) && input$y_col %in% cols) input$y_col else cols[min(2, length(cols))])
      updateSelectInput(session, "group_col", choices = c("(none)" = "", cols), selected = if (!is.null(input$group_col) && input$group_col %in% cols) input$group_col else "")
    })

    # Group values UI updates based on group column
    output$group_vals_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("group_vals"), "Group values (optional)", choices = vals, selected = vals, multiple = TRUE)
    })

    # Hull and contour groups UIs
    output$hull_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("hull_groups"), "Hull groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })
    output$contour_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("contour_groups"), "Contour groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })
    output$shape_groups_ui <- renderUI({
      df <- data_reactive(); req(df)
      gcol <- input$group_col
      if (is.null(gcol) || gcol == "" || !gcol %in% names(df)) return(NULL)
      vals <- unique(df[[gcol]])
      selectInput(ns("shape_groups"), "Shape overlay groups (optional)", choices = vals, selected = vals, multiple = TRUE)
    })

    # Export directory chooser and display
    output$export_dir_ui <- renderUI({
      if (!isTRUE(shinyfiles_ready())) return(helpText("Directory picker unavailable; using working directory"))
      shinyFiles::shinyDirButton(ns("export_dir"), label = "Choose export folder", title = "Select export folder")
    })
    export_dir_path <- reactiveVal("")
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
      shinyFiles::shinyDirChoose(input, id = "export_dir", roots = roots, session = session)
    })
    observeEvent(input$export_dir, {
      req(shinyfiles_ready())
      roots <- try(shinyFiles::getVolumes()(), silent = TRUE)
      if (inherits(roots, "try-error") || is.null(roots) || length(roots) == 0) {
        roots <- c()
      }
      if (.Platform$OS.type == "windows" && dir.exists("C:/")) {
        roots <- c(`C:` = "C:/", roots)
      }
      roots <- c(roots, Home = normalizePath("~"), `Working Dir` = normalizePath(getwd()))
      sel <- try(shinyFiles::parseDirPath(roots, input$export_dir), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) export_dir_path(as.character(sel))
    })
    output$export_dir_txt <- renderText({
      p <- export_dir_path()
      if (is.null(p) || !nzchar(p)) "No folder selected (using working directory)" else p
    })

    # Render plot
    plot_obj <- reactiveVal(NULL)
    messages <- reactiveVal("")
    legend_info <- reactiveVal(NULL)

    observeEvent(input$render, {
      df <- data_reactive()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        showNotification("No data available. Please import an Excel file in the Data Import tab.", type = "warning")
        return()
      }

      # Validate mappings
      x_col <- input$x_col; y_col <- input$y_col
      if (is.null(x_col) || !nzchar(x_col) || is.null(y_col) || !nzchar(y_col)) {
        showNotification("Please select X and Y columns.", type = "warning"); return()
      }

      gcol <- input$group_col
      gvals <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        gvals_sel <- input$group_vals
        if (!is.null(gvals_sel) && length(gvals_sel) > 0) gvals <- gvals_sel
      }

      # Build styling list from per-group pickers; fallback to defaults if none
      # Collect groups used for styling (use group filter if provided)
      style_groups <- NULL
      if (!is.null(gcol) && nzchar(gcol)) {
        style_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
      }
      # Collect per-group point colors/fills/shapes
      point_colors <- NULL
      point_fills <- NULL
      point_shapes <- NULL
      if (!is.null(style_groups) && length(style_groups)) {
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        if (isTRUE(colourpicker_ready())) {
          cols <- vapply(style_groups, function(g) input[[paste0("point_color_", safe_id(g))]] %||% NA_character_, character(1))
          fills <- vapply(style_groups, function(g) input[[paste0("point_fill_", safe_id(g))]] %||% NA_character_, character(1))
          if (any(!is.na(cols))) { point_colors <- cols[!is.na(cols)]; names(point_colors) <- as.character(style_groups[!is.na(cols)]) }
          if (any(!is.na(fills))) { point_fills <- fills[!is.na(fills)]; names(point_fills) <- as.character(style_groups[!is.na(fills)]) }
        }
        shapes <- vapply(style_groups, function(g) {
          val <- input[[paste0("point_shape_", safe_id(g))]]
          if (is.null(val)) return(NA_real_)
          # Coerce both character and numeric to numeric safely
          num <- suppressWarnings(as.numeric(val))
          if (is.na(num)) NA_real_ else num
        }, numeric(1))
        if (any(!is.na(shapes))) {
          point_shapes <- shapes[!is.na(shapes)]
          names(point_shapes) <- as.character(style_groups[!is.na(shapes)])
        }
      }

      styling <- list(
        plot_style = input$plot_style,
        point = list(
          color = point_colors,
          fill = point_fills,
          shape = point_shapes,
          size = input$point_size
        ),
        text = list(
          title_size = input$title_size,
          label_size = input$label_size,
          tick_size = input$tick_size
        ),
        axis = list(
          linewidth = input$axis_linewidth,
          tick_length = input$tick_length,
          tick_margin = input$tick_margin,
          aspect = input$aspect
        )
      )

    # Build features list
    # Choose single vs list entries for hull/contour colors using pickers if present
  # Hull single values removed; rely on per-group pickers or defaults
  hull_fill_single <- NULL
  hull_color_single <- NULL
  # No single contour color; rely on per-group pickers or defaults
  contour_color_single <- NULL

      # Collect per-group hull fill colors if provided
      hull_fill_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("hull_fill_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        # If at least one color is provided, keep vector (missing handled downstream)
        if (any(!is.na(cols))) hull_fill_by_group <- cols[!is.na(cols)]
      }
      # Collect per-group hull border colors if provided
      hull_color_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("hull_border_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        if (any(!is.na(cols))) hull_color_by_group <- cols[!is.na(cols)]
      }
      # Collect per-group contour colors if provided
      contour_color_by_group <- NULL
      if (isTRUE(colourpicker_ready()) && !is.null(input$contour_groups) && length(input$contour_groups) > 0) {
        groups <- input$contour_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        cols <- vapply(groups, function(g) {
          val <- input[[paste0("contour_color_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else val
        }, character(1))
        names(cols) <- as.character(groups)
        if (any(!is.na(cols))) contour_color_by_group <- cols[!is.na(cols)]
      }

      # Collect per-group hull alpha/linetype/linewidth
      hull_alpha_by_group <- NULL
      hull_linetype_by_group <- NULL
      hull_linewidth_by_group <- NULL
      if (!is.null(input$hull_groups) && length(input$hull_groups) > 0) {
        groups <- input$hull_groups
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        hull_alpha_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_alpha_", safe_id(g))]]
          if (is.null(val)) NA_real_ else as.numeric(val)
        }, numeric(1))
        names(hull_alpha_vals) <- as.character(groups)
        if (any(!is.na(hull_alpha_vals))) hull_alpha_by_group <- hull_alpha_vals[!is.na(hull_alpha_vals)]

        hull_linetype_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_linetype_", safe_id(g))]]
          if (is.null(val) || !nzchar(val)) NA_character_ else as.character(val)
        }, character(1))
        names(hull_linetype_vals) <- as.character(groups)
        if (any(!is.na(hull_linetype_vals))) hull_linetype_by_group <- hull_linetype_vals[!is.na(hull_linetype_vals)]

        hull_linewidth_vals <- vapply(groups, function(g) {
          val <- input[[paste0("hull_linewidth_", safe_id(g))]]
          if (is.null(val)) NA_real_ else as.numeric(val)
        }, numeric(1))
        names(hull_linewidth_vals) <- as.character(groups)
        if (any(!is.na(hull_linewidth_vals))) hull_linewidth_by_group <- hull_linewidth_vals[!is.na(hull_linewidth_vals)]
      }

      # Build hulls list without fallbacks; let shape_plot defaults apply when not provided
      hulls_list <- list(
        show = isTRUE(input$hulls_show),
        groups = input$hull_groups,
        fill = if (!is.null(hull_fill_by_group)) hull_fill_by_group else NULL,
        color = if (!is.null(hull_color_by_group)) hull_color_by_group else NULL
      )
      if (!is.null(hull_alpha_by_group)) hulls_list$alpha <- hull_alpha_by_group
      if (!is.null(hull_linetype_by_group)) hulls_list$linetype <- hull_linetype_by_group
      if (!is.null(hull_linewidth_by_group)) hulls_list$linewidth <- hull_linewidth_by_group

      features <- list(
        hulls = hulls_list,
        contours = list(
          show = isTRUE(input$contours_show),
          groups = input$contour_groups,
          colors = if (!is.null(contour_color_by_group)) contour_color_by_group else NULL,
          linewidth = input$contour_linewidth
        ),
        shapes = list(
          show = isTRUE(input$shapes_show),
          groups = input$shape_groups,
          shape_col = input$shape_col,
          only_hull = isTRUE(input$shapes_only_hull),
          size = input$shape_size,
          shift = input$shape_shift,
          x_adjust = input$shape_x_adjust,
          y_adjust = input$shape_y_adjust
        )
      )

      # Build labels list
      x_adj <- suppressWarnings(as.numeric(parse_csv(input$x_adjust)))
      y_adj <- suppressWarnings(as.numeric(parse_csv(input$y_adjust)))
      if (length(x_adj) != 2 || any(is.na(x_adj))) x_adj <- c(0, 0)
      if (length(y_adj) != 2 || any(is.na(y_adj))) y_adj <- c(0, 0)

      labels <- list(
        title = if (nzchar(input$title)) input$title else NULL,
        x_label = if (nzchar(input$x_label)) input$x_label else x_col,
        y_label = if (nzchar(input$y_label)) input$y_label else y_col,
        x_adjust = x_adj,
        y_adjust = y_adj,
        x_size = input$x_size,
        y_size = input$y_size,
        rotate_y = isTRUE(input$rotate_y),
        show_borders = isTRUE(input$show_borders)
      )

      # Build export options list
      px_w <- tryCatch({
        id <- session$ns("plot"); as.numeric(session$clientData[[paste0("output_", id, "_width")]])
      }, error = function(...) NA_real_)
      px_h <- tryCatch({
        id <- session$ns("plot"); as.numeric(session$clientData[[paste0("output_", id, "_height")]])
      }, error = function(...) NA_real_)
      export_options <- list(
        export = isTRUE(input$export),
        filename = if (nzchar(input$export_filename)) input$export_filename else "shape_plot_output",
        path = {
          p <- export_dir_path()
          if (!is.null(p) && nzchar(p)) p else NULL
        },
        format = input$export_format,
        width = if (isTRUE(input$export_custom_size)) input$export_width else NULL,
        height = if (isTRUE(input$export_custom_size)) input$export_height else NULL,
        dpi = input$export_dpi,
        match_preview = !isTRUE(input$export_custom_size),
        preview_width_px = px_w,
        preview_height_px = px_h,
        scale = input$export_scale
      )

      # Call shape_plot
      messages("")
      p <- tryCatch({
        shape_plot(
          data = df,
          x_col = x_col,
          y_col = y_col,
          group_col = if (nzchar(gcol)) gcol else NULL,
          group_vals = gvals,
          styling = styling,
          features = features,
          labels = labels,
          export_options = export_options,
          verbose = TRUE
        )
      }, error = function(e) {
        messages(paste0("Error: ", conditionMessage(e)))
        NULL
      })

      plot_obj(p)

      # Build legend info summary with actual colors and hull specimens per group
      lg <- list()
      if (!is.null(gcol) && nzchar(gcol)) {
        used_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
        used_groups <- as.character(used_groups)
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        get_or <- function(id, default) { val <- input[[id]]; if (is.null(val) || (is.character(val) && !nzchar(val))) default else val }

        # Base palette used by plotting defaults when user didn't override
        auto_pal <- tryCatch({ if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(length(used_groups)) else rep("#1f77b4", length(used_groups)) }, error = function(...) rep("#1f77b4", length(used_groups)))
        names(auto_pal) <- used_groups

        # Colors from pickers or fallback auto palette
        if (isTRUE(colourpicker_ready())) {
          cols <- setNames(vapply(used_groups, function(g) get_or(paste0("point_color_", safe_id(g)), auto_pal[[g]]), character(1)), used_groups)
          fills <- setNames(vapply(used_groups, function(g) get_or(paste0("point_fill_", safe_id(g)), auto_pal[[g]]), character(1)), used_groups)
        } else {
          cols <- auto_pal
          fills <- auto_pal
        }
        # Shapes from pickers or default 21
        shapes <- setNames(vapply(used_groups, function(g) {
          v <- input[[paste0("point_shape_", safe_id(g))]]
          if (is.null(v)) 21 else suppressWarnings(as.numeric(v))
        }, numeric(1)), used_groups)

        # Compute hull specimens only for groups that have hulls enabled/selected
        xcol <- x_col; ycol <- y_col
        hulls_on <- isTRUE(input$hulls_show)
        hull_groups <- input$hull_groups
        specimen_groups <- if (hulls_on) {
          if (!is.null(hull_groups) && length(hull_groups) > 0) intersect(used_groups, as.character(hull_groups)) else used_groups
        } else character(0)
        specimen_rows <- list()
        for (gv in specimen_groups) {
          gd <- df[df[[gcol]] == gv, , drop = FALSE]
          gd <- gd[is.finite(gd[[xcol]]) & is.finite(gd[[ycol]]), , drop = FALSE]
          if (nrow(gd) >= 3) {
            idx <- tryCatch(grDevices::chull(gd[[xcol]], gd[[ycol]]), error = function(...) integer())
            sp <- if (length(idx)) gd[idx, c(1, match(xcol, names(gd)), match(ycol, names(gd))) , drop = FALSE] else gd[0, , drop = FALSE]
          } else if (nrow(gd) > 0) {
            sp <- gd[, c(1, match(xcol, names(gd)), match(ycol, names(gd))) , drop = FALSE]
          } else {
            sp <- gd
          }
          specimen_rows[[gv]] <- sp
        }

        lg$groups <- used_groups
        lg$colors <- cols
        lg$fills <- fills
        lg$shapes <- shapes
  lg$specimens <- specimen_rows
  lg$specimen_groups <- specimen_groups
        lg$group_col <- gcol
        lg$x_col <- xcol; lg$y_col <- ycol
      }

      legend_info(lg)
    })

    output$plot <- renderPlot({
      p <- plot_obj(); req(p)
      print(p)
    })

    output$messages <- renderText({ messages() })

    output$legend_html <- renderUI({
      lg <- legend_info()
      if (is.null(lg) || is.null(lg$groups)) return(HTML("<em>No grouping applied; legend not required.</em>"))
      # Build HTML with color swatches and specimen tables per group
      rows <- list()
      rows[[length(rows)+1]] <- HTML(paste0("<div><strong>Grouping by:</strong> ", htmltools::htmlEscape(lg$group_col), "</div>"))
      for (g in lg$groups) {
        col <- lg$colors[[g]]; fill <- lg$fills[[g]]; shp <- lg$shapes[[g]]
        swatch <- paste0('<span style="display:inline-block;width:14px;height:14px;border:1px solid #888;background:', htmltools::htmlEscape(fill), ';"></span>')
        header <- HTML(paste0("<div style=\"margin-top:6px;\">", swatch, " <strong>", htmltools::htmlEscape(col), "</strong>: group ", htmltools::htmlEscape(g), " (shape ", htmltools::htmlEscape(as.character(shp)), ")</div>"))
        rows[[length(rows)+1]] <- header
        # Specimen table
        sp <- lg$specimens[[g]]
        if (g %in% (lg$specimen_groups %||% character(0)) && !is.null(sp) && nrow(sp) > 0) {
          # Ensure columns have names: first col is ID (guess), then x,y
          colnames(sp)[1:3] <- c("ID", lg$x_col, lg$y_col)
          # Build compact HTML table
          tbl_head <- paste0("<table class=\"table table-sm\" style=\"margin-left:18px;\"><thead><tr><th>ID</th><th>", htmltools::htmlEscape(lg$x_col), "</th><th>", htmltools::htmlEscape(lg$y_col), "</th></tr></thead><tbody>")
          tbl_rows <- apply(sp, 1, function(r) {
            sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", htmltools::htmlEscape(as.character(r[1])), htmltools::htmlEscape(format(as.numeric(r[2]), digits = 4)), htmltools::htmlEscape(format(as.numeric(r[3]), digits = 4)))
          })
          tbl <- HTML(paste0(tbl_head, paste(tbl_rows, collapse = ""), "</tbody></table>"))
          rows[[length(rows)+1]] <- tbl
        }
      }
      do.call(tagList, rows)
    })

    # Preview size readout (px) from client data
    preview_px <- reactive({
      id <- session$ns("plot")
      w <- session$clientData[[paste0("output_", id, "_width")]]
      h <- session$clientData[[paste0("output_", id, "_height")]]
      list(w = as.numeric(w %||% NA_real_), h = as.numeric(h %||% NA_real_))
    })
    output$preview_size_txt <- renderText({
      px <- preview_px()
      if (is.finite(px$w) && is.finite(px$h)) sprintf("Preview size: %d × %d px", as.integer(px$w), as.integer(px$h)) else "Preview size: (detecting...)"
    })

      # Download handler: ggplot object as .rds for further editing in RStudio
      output$download_plot_rds <- downloadHandler(
        filename = function() {
          stem <- input$export_filename
          if (is.null(stem) || !nzchar(stem)) stem <- "shape_plot_output"
          paste0(stem, ".rds")
        },
        content = function(file) {
          p <- plot_obj()
          validate(need(!is.null(p), "No plot has been rendered yet. Click 'Render plot' first."))
          saveRDS(p, file)
        }
      )

    # Removed: hull specimens modal button and observer (now shown inline in the legend)

    invisible(list(plot = plot_obj))
  })
}
