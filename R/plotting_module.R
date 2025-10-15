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
          selectInput(ns("point_shape_select"), "Point shape (single)", choices = c(
            "Circle filled (21)" = 21,
            "Circle solid (16)" = 16,
            "Square filled (22)" = 22,
            "Diamond filled (23)" = 23,
            "Triangle up (24)" = 24,
            "Circle open (1)" = 1,
            "Square open (0)" = 0
          ), selected = 21),
          textInput(ns("point_shape"), "Point shape(s) per-group (comma-separated, overrides single)", value = ""),
          textInput(ns("point_color"), "Point color(s) (comma-separated)", value = "#1f77b4"),
          uiOutput(ns("point_color_picker")),
          textInput(ns("point_fill"), "Point fill color(s) (comma-separated)", value = "#1f77b4"),
          uiOutput(ns("point_fill_picker")),
          numericInput(ns("title_size"), "Title size", value = 24, min = 6, step = 1),
          numericInput(ns("label_size"), "Axis label size", value = 20, min = 6, step = 1),
          numericInput(ns("tick_size"), "Tick label size", value = 15, min = 6, step = 1),
          numericInput(ns("axis_linewidth"), "Axis linewidth", value = 1, min = 0, step = 0.25),
          numericInput(ns("tick_length"), "Tick length (npc)", value = 0.005, min = 0, step = 0.001),
          numericInput(ns("tick_margin"), "Tick margin", value = 0.05, min = 0, step = 0.01)
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
          # Per-group hull fill color pickers (if a group column is selected)
          uiOutput(ns("hull_group_fill_pickers")),
          uiOutput(ns("hull_group_border_pickers")),
          numericInput(ns("hull_alpha"), "Hull alpha", value = 0.1, min = 0, max = 1, step = 0.05),
          selectInput(ns("hull_linetype"), "Hull linetype", choices = c("solid","dashed","dotted","dotdash","longdash","twodash"), selected = "solid")
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
          # Per-group contour color pickers
          uiOutput(ns("contour_group_color_pickers")),
          uiOutput(ns("contour_color_picker")),
          # Fallback single color text input when colourpicker is unavailable
          textInput(ns("contour_color_single_text"), "Contour color (single) [fallback]", value = "black"),
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
          textInput(ns("export_path"), "Export path (leave empty to use working dir)", value = ""),
          selectInput(ns("export_format"), "Format", choices = c("tiff","png","jpg","pdf"), selected = "tiff"),
          numericInput(ns("export_width"), "Width (inches)", value = 10, min = 1, step = 0.5),
          numericInput(ns("export_height"), "Height (inches)", value = 8, min = 1, step = 0.5),
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
      )
    )
  )
}

#' @export
plotting_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # Render color pickers if available
    output$point_color_picker <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      colourpicker::colourInput(ns("point_color_single"), "Point color (single)", value = "#1f77b4")
    })
    output$point_fill_picker <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      colourpicker::colourInput(ns("point_fill_single"), "Point fill (single)", value = "#1f77b4")
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
    output$contour_color_picker <- renderUI({
      if (!isTRUE(colourpicker_ready())) return(NULL)
      colourpicker::colourInput(ns("contour_color_single"), "Contour color (single)", value = "black")
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
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", as.character(x))
      picker_list <- lapply(groups, function(g) {
        inputId <- ns(paste0("contour_color_", safe_id(g)))
        label <- paste0("Contour color: ", g)
        colourpicker::colourInput(inputId, label, value = "black")
      })
      do.call(tagList, picker_list)
    })

    # Helper: parse comma-separated values into vector, trim whitespace
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

    # Render plot
    plot_obj <- reactiveVal(NULL)
    messages <- reactiveVal("")

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

      # Build styling list
  point_shapes <- parse_csv(input$point_shape)
  point_shapes_num <- suppressWarnings(as.numeric(point_shapes))
  if (all(!is.na(point_shapes_num))) point_shapes <- point_shapes_num

      # Choose single vs per-group shapes
      final_shape <- if (length(point_shapes) > 0) point_shapes else as.numeric(input$point_shape_select)

      # Prefer colourpicker single selections when available
  point_color_single <- if (isTRUE(colourpicker_ready()) && !is.null(input$point_color_single) && nzchar(input$point_color_single)) input$point_color_single else NULL
  point_fill_single  <- if (isTRUE(colourpicker_ready()) && !is.null(input$point_fill_single)  && nzchar(input$point_fill_single))  input$point_fill_single  else NULL

      styling <- list(
        plot_style = input$plot_style,
        point = list(
          color = if (!is.null(point_color_single)) point_color_single else if (length(parse_csv(input$point_color)) > 0) parse_csv(input$point_color) else NULL,
          fill = if (!is.null(point_fill_single)) point_fill_single else if (length(parse_csv(input$point_fill)) > 0) parse_csv(input$point_fill) else NULL,
          shape = final_shape,
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
          tick_margin = input$tick_margin
        )
      )

    # Build features list
    # Choose single vs list entries for hull/contour colors using pickers if present
  # Hull single values removed; rely on per-group pickers or defaults
  hull_fill_single <- NULL
  hull_color_single <- NULL
  contour_color_single <- if (isTRUE(colourpicker_ready()) && !is.null(input$contour_color_single) && nzchar(input$contour_color_single)) input$contour_color_single else if (!isTRUE(colourpicker_ready()) && nzchar(input$contour_color_single_text)) input$contour_color_single_text else NULL

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

      features <- list(
        hulls = list(
          show = isTRUE(input$hulls_show),
          groups = input$hull_groups,
          fill = if (!is.null(hull_fill_by_group)) hull_fill_by_group else NULL,
          color = if (!is.null(hull_color_by_group)) hull_color_by_group else NULL,
          alpha = input$hull_alpha,
          linetype = input$hull_linetype
        ),
        contours = list(
          show = isTRUE(input$contours_show),
          groups = input$contour_groups,
          colors = if (!is.null(contour_color_by_group)) contour_color_by_group else if (!is.null(contour_color_single)) contour_color_single else NULL,
          linewidth = input$contour_linewidth
        ),
        shapes = list(
          show = isTRUE(input$shapes_show),
          groups = input$shape_groups,
          shape_col = input$shape_col,
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
        path = if (nzchar(input$export_path)) input$export_path else NULL,
        format = input$export_format,
        width = input$export_width,
        height = input$export_height,
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
    })

    output$plot <- renderPlot({
      p <- plot_obj(); req(p)
      print(p)
    })

    output$messages <- renderText({ messages() })

    invisible(list(plot = plot_obj))
  })
}
