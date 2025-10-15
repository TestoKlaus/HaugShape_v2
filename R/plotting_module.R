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
          selectInput(ns("aspect"), "Plot aspect ratio", choices = c("2:1","1:1"), selected = "2:1")
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
            ns = ns,
            tagList(
              numericInput(ns("export_width"), "Width (inches)", value = 8, min = 1, step = 0.5),
              numericInput(ns("export_height"), "Height (inches)", value = 8, min = 1, step = 0.5)
            )
          ),
          numericInput(ns("export_dpi"), "DPI", value = 300, min = 72, step = 10)
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
          verbatimTextOutput(ns("legend_text")),
          div(style = "margin-top:8px;",
              actionButton(ns("show_hull_specimens"), "Show hull specimens", class = "btn-primary")
          )
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
        dpi = input$export_dpi
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

      # Build legend info summary
      # Determine used groups and aesthetic mappings applied
      lg <- list()
      if (!is.null(gcol) && nzchar(gcol)) {
        used_groups <- if (!is.null(gvals) && length(gvals)) gvals else unique(df[[gcol]])
        # Build per-group aesthetics from input pickers if present; fallback to automatic palette
        safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
        get_or <- function(id, default) { val <- input[[id]]; if (is.null(val) || (is.character(val) && !nzchar(val))) default else val }
        # Colors
        if (isTRUE(colourpicker_ready())) {
          cols <- setNames(vapply(used_groups, function(g) get_or(paste0("point_color_", safe_id(g)), NA_character_), character(1)), as.character(used_groups))
          fills <- setNames(vapply(used_groups, function(g) get_or(paste0("point_fill_", safe_id(g)), NA_character_), character(1)), as.character(used_groups))
        } else {
          cols <- setNames(rep(NA_character_, length(used_groups)), as.character(used_groups))
          fills <- cols
        }
        # Shapes
        shapes <- setNames(vapply(used_groups, function(g) {
          v <- input[[paste0("point_shape_", safe_id(g))]]
          if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
        }, numeric(1)), as.character(used_groups))

        lg$groups <- used_groups
        lg$colors <- cols
        lg$fills <- fills
        lg$shapes <- shapes
        lg$group_col <- gcol
      }

      legend_info(lg)
    })

    output$plot <- renderPlot({
      p <- plot_obj(); req(p)
      print(p)
    })

    output$messages <- renderText({ messages() })

    output$legend_text <- renderText({
      lg <- legend_info()
      if (is.null(lg) || is.null(lg$groups)) return("No grouping applied; legend not required.")
      fmt_shape <- function(s) ifelse(is.na(s), "(default)", as.character(s))
      fmt_col <- function(c) ifelse(is.na(c) || !nzchar(c), "(auto)", c)
      lines <- c(
        paste0("Grouping by: ", lg$group_col),
        paste0("Groups (n=", length(lg$groups), "):")
      )
      for (g in lg$groups) {
        lines <- c(lines, paste0("  - ", g, ": color=", fmt_col(lg$colors[[as.character(g)]]), ", fill=", fmt_col(lg$fills[[as.character(g)]]), ", shape=", fmt_shape(lg$shapes[[as.character(g)]])))
      }
      paste(lines, collapse = "\n")
    })

    observeEvent(input$show_hull_specimens, {
      df <- data_reactive(); req(df)
      gcol <- input$group_col; req(nzchar(gcol))
      x_col <- input$x_col; y_col <- input$y_col
      gvals <- input$group_vals
      # Default groups if none explicitly selected
      if (is.null(gvals) || !length(gvals)) gvals <- unique(df[[gcol]])
      res <- try(get_hull_specimen_table(
        data = df,
        x_cols = x_col,
        y_cols = y_col,
        group_col = gcol,
        group_vals = gvals,
        verbose = FALSE
      ), silent = TRUE)
      if (inherits(res, "try-error")) {
        showModal(modalDialog(title = "Hull specimens", "Failed to compute hull specimens."))
        return()
      }
      # Prefer a light textual summary if ggpubr tables aren't available
      text_summary <- NULL
      if (!requireNamespace("ggpubr", quietly = TRUE)) {
        sumdf <- res$summary
        text_summary <- paste0(
          "Total sections: ", if (!is.null(sumdf) && nrow(sumdf)) sumdf$value[sumdf$metric == "Total sections"] else NA,
          "\nTotal rows: ", if (!is.null(sumdf) && nrow(sumdf)) sumdf$value[sumdf$metric == "Total rows"] else NA
        )
      }
      ui <- if (!is.null(text_summary)) {
        tagList(pre(text_summary))
      } else {
        # ggpubr tables exist; render the first page to keep it small
        if (length(res$tables) > 0 && inherits(res$tables[[1]], "ggplot")) {
          plotOutput(ns("hull_table_plot"), height = 300)
        } else {
          pre("No table available.")
        }
      }
      showModal(modalDialog(title = "Hull specimens (first page)", size = "l", easyClose = TRUE, footer = modalButton("Close"), ui))
      if (length(res$tables) > 0 && inherits(res$tables[[1]], "ggplot")) {
        output$hull_table_plot <- renderPlot({ print(res$tables[[1]]) })
      }
    })

    invisible(list(plot = plot_obj))
  })
}
