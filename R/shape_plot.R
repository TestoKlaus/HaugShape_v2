#' Generate a Customizable Scatter Plot with Hulls and Contours
#'
#' Creates a highly customizable scatter plot with optional convex hulls, 
#' contours, and shape overlays. Supports grouping, multiple styling options,
#' and various export formats.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_col Character string specifying the x-axis column name.
#' @param y_col Character string specifying the y-axis column name.
#' @param group_col Optional character string specifying the grouping column name.
#' @param group_vals Optional vector specifying which group values to display.
#'   If NULL, all unique values in group_col are used.
#' @param styling List containing styling options. See Details for available options.
#' @param features List containing feature options (hulls, contours, shapes). 
#'   See Details for available options.
#' @param labels List containing label options (title, axis labels).
#' @param export_options List containing export options. See Details for available options.
#' @param verbose Logical indicating whether to print progress messages. Default: TRUE.
#'
#' @details
#' The `styling` parameter accepts a list with the following options:
#' \describe{
#'   \item{plot_style}{Style theme: "Haug", "inverted_Haug", "publication" (default: "Haug")}
#'   \item{point}{List with point styling (color, fill, shape, size)}
#'   \item{text}{List with text styling (title_size, label_size, tick_size)}
#'   \item{axis}{List with axis styling (linewidth, tick_length, tick_margin)}
#' }
#'
#' The `features` parameter accepts a list with the following options:
#' \describe{
#'   \item{hulls}{List with hull options (show, groups, fill, color, alpha, linetype)}
#'   \item{contours}{List with contour options (show, groups, colors, linewidth)}
#'   \item{shapes}{List with shape options (show, groups, size, shift, adjustments)}
#' }
#'
#' @return A ggplot2 object representing the scatter plot.
#'
#' @examples
#' # Basic scatter plot
#' test_data <- data.frame(
#'   PC1 = rnorm(100), PC2 = rnorm(100),
#'   species = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Simple plot
#' p1 <- shape_plot(test_data, x_col = "PC1", y_col = "PC2")
#'
#' # Grouped plot with hulls
#' p2 <- shape_plot(
#'   data = test_data,
#'   x_col = "PC1", y_col = "PC2", group_col = "species",
#'   features = list(hulls = list(show = TRUE))
#' )
#'
#' # Advanced customization
#' p3 <- shape_plot(
#'   data = test_data,
#'   x_col = "PC1", y_col = "PC2", group_col = "species",
#'   styling = list(
#'     plot_style = "publication",
#'     point = list(size = 3, shape = 16),
#'     text = list(title_size = 20)
#'   ),
#'   features = list(
#'     hulls = list(show = TRUE, alpha = 0.3),
#'     contours = list(show = TRUE)
#'   ),
#'   labels = list(
#'     title = "PCA Analysis",
#'     x_label = "First Principal Component",
#'     y_label = "Second Principal Component"
#'   )
#' )
#'
#' @export
shape_plot <- function(data,
                      x_col,
                      y_col,
                      group_col = NULL,
                      group_vals = NULL,
                      styling = list(),
                      features = list(),
                      labels = list(),
                      export_options = list(),
                      verbose = TRUE) {
  
  # Input validation ----
  .validate_shape_plot_inputs(data, x_col, y_col, group_col, group_vals, verbose)
  
  # Setup parameters with defaults ----
  params <- .setup_shape_plot_params(
    data, x_col, y_col, group_col, group_vals, 
    styling, features, labels, export_options, verbose
  )
  
  # Clean and prepare data ----
  clean_data <- .prepare_plot_data(data, x_col, y_col, group_col, params$features$shapes$show, 
                                  params$features$shapes$shape_col, verbose)
  
  if (verbose) {
    message("Creating shape plot with ", nrow(clean_data), " data points")
    if (!is.null(group_col)) {
      message("Groups: ", paste(params$group_vals, collapse = ", "))
    }
  }
  
  # Create base plot ----
  plot <- .create_base_plot(clean_data, x_col, y_col, params)
  
  # Add points ----
  plot <- .add_points_to_plot(plot, clean_data, x_col, y_col, group_col, params)
  
  # Add features ----
  if (params$features$hulls$show) {
    plot <- .add_hulls_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  if (params$features$contours$show) {
    plot <- .add_contours_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  if (params$features$shapes$show) {
    plot <- .add_shapes_to_plot(plot, clean_data, x_col, y_col, group_col, params, verbose)
  }
  
  # Apply styling and theming ----
  plot <- .apply_plot_styling(plot, params)
  # Centralized axes overlay (legacy style)
  if (isTRUE(params$styling$axis$central_axes)) {
    plot <- .apply_central_axes(plot, clean_data, x_col, y_col, params)
  }
  
  # Export if requested ----
  if (params$export_options$export) {
    .export_plot(plot, params$export_options, verbose)
  }
  
  return(plot)
}

# Input Validation ----

#' Validate inputs for shape_plot function
#' @noRd
.validate_shape_plot_inputs <- function(data, x_col, y_col, group_col, group_vals, verbose) {
  # Check required parameters
  if (missing(data)) stop("'data' is required", call. = FALSE)
  if (missing(x_col)) stop("'x_col' is required", call. = FALSE)  
  if (missing(y_col)) stop("'y_col' is required", call. = FALSE)
  
  # Check data
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("'data' cannot be empty", call. = FALSE)
  }
  
  # Check column existence
  if (!x_col %in% colnames(data)) {
    stop("Column '", x_col, "' does not exist in data", call. = FALSE)
  }
  if (!y_col %in% colnames(data)) {
    stop("Column '", y_col, "' does not exist in data", call. = FALSE)
  }
  
  # Check group_col
  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1) {
      stop("'group_col' must be a single character string", call. = FALSE)
    }
    if (!group_col %in% colnames(data)) {
      stop("Column '", group_col, "' does not exist in data", call. = FALSE)
    }
  }
  
  # Check group_vals
  if (!is.null(group_vals) && !is.null(group_col)) {
    if (!all(group_vals %in% unique(data[[group_col]]))) {
      missing_vals <- group_vals[!group_vals %in% unique(data[[group_col]])]
      stop("The following group values do not exist: ", paste(missing_vals, collapse = ", "), 
           call. = FALSE)
    }
  }
  
  # Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value", call. = FALSE)
  }
}

# Parameter Setup ----

#' Setup default parameters for shape_plot
#' @noRd
.setup_shape_plot_params <- function(data, x_col, y_col, group_col, group_vals,
                                    styling, features, labels, export_options, verbose) {
  
  # Setup group values
  if (!is.null(group_col) && is.null(group_vals)) {
    group_vals <- unique(data[[group_col]])
  }
  
  # Setup colors
  n_groups <- if (!is.null(group_vals)) length(group_vals) else 1
  default_colors <- if (n_groups > 1) scales::hue_pal()(n_groups) else c("#1f77b4")
  
  # Setup styling defaults
  styling_defaults <- list(
    plot_style = "Haug",
    point = list(
      color = default_colors,
      fill = default_colors,
      shape = 21,
      size = 2
    ),
    text = list(
      title_size = 24,
      label_size = 20,
      tick_size = 15
    ),
    axis = list(
      linewidth = 1,
      tick_length = 0.005,
      tick_margin = 0.05,
      central_axes = TRUE
    )
  )
  styling <- .merge_nested_lists(styling_defaults, styling)
  
  # Setup feature defaults
  features_defaults <- list(
    hulls = list(
      show = FALSE,
      groups = group_vals,
      fill = default_colors,
      color = "black",
      alpha = 0.1,
      linetype = "solid"
    ),
    contours = list(
      show = FALSE,
      groups = group_vals,
      colors = "black",
      linewidth = 0.5
    ),
    shapes = list(
      show = FALSE,
      groups = group_vals,
      shape_col = "shape",
      size = 0.01,
      shift = 0.1,
      x_adjust = 0,
      y_adjust = 0
    )
  )
  features <- .merge_nested_lists(features_defaults, features)
  # Ensure non-NULL defaults after merge (avoid replication errors)
  if (is.null(features$hulls$fill) || length(features$hulls$fill) == 0) {
    features$hulls$fill <- default_colors
  }
  if (is.null(features$hulls$color) || length(features$hulls$color) == 0) {
    features$hulls$color <- "black"
  }
  if (is.null(features$contours$colors) || length(features$contours$colors) == 0) {
    features$contours$colors <- "black"
  }
  
  # Setup label defaults
  labels_defaults <- list(
    title = NULL,
    x_label = x_col,
    y_label = y_col,
    x_adjust = c(0, 0),  # c(x, y) adjustments
    y_adjust = c(0, 0),  # c(x, y) adjustments
    x_size = 5,
    y_size = 5,
    rotate_y = FALSE,
    show_borders = TRUE
  )
  labels <- utils::modifyList(labels_defaults, labels)
  
  # Setup export defaults
  export_defaults <- list(
    export = FALSE,
    filename = "shape_plot_output",
    path = NULL,
    format = "tiff",
    width = 10,
    height = 8,
    dpi = 300
  )
  export_options <- utils::modifyList(export_defaults, export_options)
  
  return(list(
    group_vals = group_vals,
    styling = styling,
    features = features,
    labels = labels,
    export_options = export_options
  ))
}

#' Merge nested lists recursively
#' @noRd
.merge_nested_lists <- function(default, user) {
  for (name in names(user)) {
    if (name %in% names(default) && is.list(default[[name]]) && is.list(user[[name]])) {
      default[[name]] <- utils::modifyList(default[[name]], user[[name]])
    } else {
      default[[name]] <- user[[name]]
    }
  }
  return(default)
}

#' Resolve a vector (possibly named) to match a set of groups
#' @noRd
.resolve_group_vector <- function(vec, groups, fallback_fn = NULL) {
  groups_chr <- as.character(groups)
  # If vec is NULL or length 0, use fallback
  if (is.null(vec) || length(vec) == 0) {
    if (is.null(fallback_fn)) return(rep_len("#1f77b4", length(groups_chr)))
    return(fallback_fn(length(groups_chr)))
  }
  # If named, align by names
  nm <- names(vec)
  if (!is.null(nm) && any(nzchar(nm))) {
    out <- vapply(groups_chr, function(g) {
      if (g %in% nm) vec[[g]] else NA_character_
    }, character(1))
    # Fill missing with rep_len of first (or fallback)
    if (anyNA(out)) {
      repl <- if (!is.null(fallback_fn)) fallback_fn(sum(is.na(out))) else rep_len(vec[[1]], sum(is.na(out)))
      out[is.na(out)] <- repl
    }
    return(out)
  }
  # Otherwise, just recycle
  return(rep_len(vec, length(groups_chr)))
}

# Data Preparation ----

#' Prepare and clean plot data
#' @noRd
.prepare_plot_data <- function(data, x_col, y_col, group_col, show_shapes, shape_col, verbose) {
  
  # Define required columns
  required_cols <- c(x_col, y_col)
  if (!is.null(group_col)) required_cols <- c(required_cols, group_col)
  if (show_shapes && shape_col %in% colnames(data)) required_cols <- c(required_cols, shape_col)
  
  # Filter out rows with missing or infinite values
  clean_data <- data %>%
    dplyr::filter(
      !is.na(.data[[x_col]]) & is.finite(.data[[x_col]]) &
      !is.na(.data[[y_col]]) & is.finite(.data[[y_col]]) &
      if (!is.null(group_col)) !is.na(.data[[group_col]]) else TRUE &
      if (show_shapes && shape_col %in% colnames(data)) !is.na(.data[[shape_col]]) else TRUE
    )
  
  rows_removed <- nrow(data) - nrow(clean_data)
  if (verbose && rows_removed > 0) {
    message("Removed ", rows_removed, " rows with missing or infinite values")
  }
  
  if (nrow(clean_data) == 0) {
    stop("No valid data points remaining after filtering", call. = FALSE)
  }
  
  return(clean_data)
}

# Plot Creation ----

#' Create base ggplot object
#' @noRd
.create_base_plot <- function(data, x_col, y_col, params) {
  
  # Get style colors
  style_colors <- .get_style_colors(params$styling$plot_style)
  
  # Create base plot
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_col, y = y_col))
  
  # Add background rectangle for publication style
  if (params$styling$plot_style == "publication") {
    x_range <- range(data[[x_col]], na.rm = TRUE)
    y_range <- range(data[[y_col]], na.rm = TRUE)
    
    plot <- plot +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = x_range[1], xmax = x_range[2], 
                    ymin = y_range[1], ymax = y_range[2]),
        fill = style_colors$plot_background, color = NA, inherit.aes = FALSE
      )
  }
  
  return(plot)
}

#' Get style-specific colors
#' @noRd
.get_style_colors <- function(plot_style) {
  switch(plot_style,
    "inverted_Haug" = list(
      background = "black",
      plot_background = "black", 
      text = "white",
      axis = "white",
      text_field_fill = "black",
      text_field_color = "white"
    ),
    "publication" = list(
      background = "white",
      plot_background = "#f1f1f1",
      text = "black", 
      axis = "black",
      text_field_fill = "white",
      text_field_color = "black"
    ),
    "Haug" = list(
      background = "white",
      plot_background = "white",
      text = "black",
      axis = "black", 
      text_field_fill = "white",
      text_field_color = "black"
    )
  )
}

# Point Addition ----

#' Add points to the plot
#' @noRd
.add_points_to_plot <- function(plot, data, x_col, y_col, group_col, params) {
  
  if (!is.null(group_col) && !is.null(params$group_vals)) {
    # Add grouped points
    point_colors <- rep_len(params$styling$point$color, length(params$group_vals))
    point_fills <- rep_len(params$styling$point$fill, length(params$group_vals))
    point_shapes <- rep_len(params$styling$point$shape, length(params$group_vals))
    point_sizes <- rep_len(params$styling$point$size, length(params$group_vals))
    
    for (i in seq_along(params$group_vals)) {
      group_val <- params$group_vals[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (nrow(group_data) > 0) {
        plot <- plot +
          ggplot2::geom_point(
            data = group_data,
            ggplot2::aes_string(x = x_col, y = y_col),
            color = point_colors[i],
            fill = point_fills[i], 
            shape = point_shapes[i],
            size = point_sizes[i]
          )
      }
    }
  } else {
    # Add ungrouped points
    plot <- plot +
      ggplot2::geom_point(
        ggplot2::aes_string(x = x_col, y = y_col),
        color = params$styling$point$color[1],
        fill = params$styling$point$fill[1],
        shape = params$styling$point$shape[1], 
        size = params$styling$point$size[1]
      )
  }
  
  return(plot)
}

# Hull Addition ----

#' Add convex hulls to the plot
#' @noRd
.add_hulls_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {
  
  if (is.null(group_col)) {
    # Single hull for all data
    if (nrow(data) >= 3) {
      hull_indices <- grDevices::chull(data[[x_col]], data[[y_col]])
      hull_data <- data[hull_indices, ]
      
      plot <- plot +
        ggplot2::geom_polygon(
          data = hull_data,
          ggplot2::aes_string(x = x_col, y = y_col),
          fill = params$features$hulls$fill[1],
          color = params$features$hulls$color[1],
          alpha = params$features$hulls$alpha[1],
          linetype = params$features$hulls$linetype[1]
        )
    } else if (verbose) {
      warning("Insufficient points for hull calculation (need >= 3)")
    }
  } else {
    # Group-specific hulls
    hull_groups <- params$features$hulls$groups
    if (is.null(hull_groups)) hull_groups <- params$group_vals
    
    # Resolve per-group aesthetics (supports named vectors)
    hull_fills <- .resolve_group_vector(
      params$features$hulls$fill,
      hull_groups,
      function(n) { if (requireNamespace("scales", quietly = TRUE)) scales::hue_pal()(n) else rep("#1f77b4", n) }
    )
    hull_colors <- .resolve_group_vector(
      params$features$hulls$color,
      hull_groups,
      function(n) rep("black", n)
    )
    hull_alphas <- rep_len(params$features$hulls$alpha, length(hull_groups))
    
    for (i in seq_along(hull_groups)) {
      group_val <- hull_groups[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (verbose) {
        message("Processing hull for group: ", group_val, " (", nrow(group_data), " points)")
      }
      
      if (nrow(group_data) >= 3) {
        tryCatch({
          hull_indices <- grDevices::chull(group_data[[x_col]], group_data[[y_col]])
          hull_data <- group_data[hull_indices, ]
          
          plot <- plot +
            ggplot2::geom_polygon(
              data = hull_data,
              ggplot2::aes_string(x = x_col, y = y_col),
              fill = hull_fills[i],
              color = hull_colors[i],
              alpha = hull_alphas[i],
              linetype = params$features$hulls$linetype[1]
            )
        }, error = function(e) {
          if (verbose) warning("Failed to create hull for group ", group_val, ": ", e$message)
        })
      } else if (verbose) {
        warning("Group ", group_val, " has < 3 points, skipping hull")
      }
    }
  }
  
  return(plot)
}

# Contour Addition ----

#' Add contours to the plot
#' @noRd
.add_contours_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {
  
  if (!requireNamespace("MASS", quietly = TRUE)) {
    if (verbose) warning("Package 'MASS' required for contours but not available")
    return(plot)
  }
  
  contour_groups <- params$features$contours$groups
  if (is.null(group_col) || is.null(contour_groups)) {
    # Single contour for all data
    tryCatch({
      kde_result <- MASS::kde2d(data[[x_col]], data[[y_col]], n = 50)
      contour_data <- .kde_to_dataframe(kde_result)
      
      plot <- plot +
        ggplot2::geom_contour(
          data = contour_data,
          ggplot2::aes(x = x, y = y, z = z),
          color = params$features$contours$colors[1],
          size = params$features$contours$linewidth
        )
    }, error = function(e) {
      if (verbose) warning("Failed to create contours: ", e$message)
    })
  } else {
    # Group-specific contours
    contour_colors <- .resolve_group_vector(
      params$features$contours$colors,
      contour_groups,
      function(n) rep("black", n)
    )
    
    for (i in seq_along(contour_groups)) {
      group_val <- contour_groups[i]
      group_data <- data %>% dplyr::filter(!!rlang::sym(group_col) == group_val)
      
      if (nrow(group_data) >= 10) {  # Need sufficient points for KDE
        tryCatch({
          kde_result <- MASS::kde2d(group_data[[x_col]], group_data[[y_col]], n = 50)
          contour_data <- .kde_to_dataframe(kde_result)
          
          plot <- plot +
            ggplot2::geom_contour(
              data = contour_data,
              ggplot2::aes(x = x, y = y, z = z),
              color = contour_colors[i],
              size = params$features$contours$linewidth
            )
        }, error = function(e) {
          if (verbose) warning("Failed to create contours for group ", group_val, ": ", e$message)
        })
      } else if (verbose) {
        warning("Group ", group_val, " has insufficient points for contours (need >= 10)")
      }
    }
  }
  
  return(plot)
}

#' Convert KDE result to data frame
#' @noRd
.kde_to_dataframe <- function(kde_result) {
  expand.grid(x = kde_result$x, y = kde_result$y) %>%
    dplyr::mutate(z = as.vector(kde_result$z))
}

# Shape Addition ----

#' Add shapes to the plot
#' @noRd
.add_shapes_to_plot <- function(plot, data, x_col, y_col, group_col, params, verbose) {
  
  shape_col <- params$features$shapes$shape_col
  
  if (!shape_col %in% colnames(data)) {
    if (verbose) warning("Shape column '", shape_col, "' not found in data")
    return(plot)
  }
  
  # This is a complex feature that would need the actual shape data structure
  # For now, adding a placeholder that can be extended based on your shape data format
  if (verbose) message("Shape overlay feature needs implementation based on your shape data structure")
  
  return(plot)
}

# Styling Application ----

#' Apply styling to the plot
#' @noRd
.apply_plot_styling <- function(plot, params) {
  
  style_colors <- .get_style_colors(params$styling$plot_style)
  
  plot <- plot +
    ggplot2::labs(
      title = params$labels$title,
      x = params$labels$x_label,
      y = params$labels$y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = style_colors$background, color = NA),
      panel.background = ggplot2::element_rect(fill = style_colors$plot_background, color = NA),
      plot.title = ggplot2::element_text(
        size = params$styling$text$title_size, 
        color = style_colors$text
      ),
      axis.title = ggplot2::element_text(
        size = params$styling$text$label_size,
        color = style_colors$text
      ),
      axis.text = ggplot2::element_text(
        size = params$styling$text$tick_size,
        color = style_colors$text
      ),
      axis.line = ggplot2::element_line(
        color = style_colors$axis,
        size = params$styling$axis$linewidth
      ),
      axis.ticks = ggplot2::element_line(
        color = style_colors$axis,
        size = params$styling$axis$linewidth
      ),
      axis.ticks.length = ggplot2::unit(params$styling$axis$tick_length, "npc")
    )
  
  return(plot)
}

#' Add centralized axes with arrows, custom ticks, and label fields (legacy style)
#' @noRd
.apply_central_axes <- function(plot, data, x_col, y_col, params) {
  # Ranges and padding
  x_range <- range(data[[x_col]], na.rm = TRUE)
  y_range <- range(data[[y_col]], na.rm = TRUE)
  x_expand <- 0.05 * (x_range[2] - x_range[1])
  y_expand <- 0.05 * (y_range[2] - y_range[1])

  # Ticks (exclude 0 and outermost)
  x_ticks <- pretty(x_range)
  y_ticks <- pretty(y_range)
  x_ticks <- x_ticks[x_ticks != 0 & x_ticks > x_range[1] & x_ticks < x_range[2]]
  y_ticks <- y_ticks[y_ticks != 0 & y_ticks > y_range[1] & y_ticks < y_range[2]]

  # Styling
  style_colors <- .get_style_colors(params$styling$plot_style)
  axis_col <- style_colors$axis
  text_col <- style_colors$text
  lw <- params$styling$axis$linewidth
  tick_len_prop <- params$styling$axis$tick_length
  tick_margin <- params$styling$axis$tick_margin

  # Convert tick length to data units
  x_tick_len <- tick_len_prop * diff(y_range)
  y_tick_len <- tick_len_prop * diff(x_range)

  # Reset/minimal theme with hidden default axes/labels
  plot <- plot +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(tick_margin, tick_margin, tick_margin, tick_margin)
    )

  # Arrowed axes through origin
  plot <- plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = x_range[1] - x_expand, xend = x_range[2] + x_expand, y = 0, yend = 0),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_col, size = lw
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(y = y_range[1] - y_expand, yend = y_range[2] + y_expand, x = 0, xend = 0),
      arrow = grid::arrow(length = grid::unit(0.3, "cm")), color = axis_col, size = lw
    )

  # Tick marks
  if (length(x_ticks)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(x = x_ticks),
        ggplot2::aes(x = x, xend = x, y = -x_tick_len, yend = x_tick_len),
        color = axis_col, size = lw
      )
  }
  if (length(y_ticks)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = data.frame(y = y_ticks),
        ggplot2::aes(y = y, yend = y, x = -y_tick_len, xend = y_tick_len),
        color = axis_col, size = lw
      )
  }

  # Tick labels
  tick_text_size <- params$styling$text$tick_size / 3
  if (length(x_ticks)) {
    plot <- plot +
      ggplot2::geom_text(
        data = data.frame(x = x_ticks, y = 0),
        ggplot2::aes(x = x, y = y, label = x),
        vjust = 1.5 + tick_len_prop * 50,
        size = tick_text_size, color = text_col
      )
  }
  if (length(y_ticks)) {
    plot <- plot +
      ggplot2::geom_text(
        data = data.frame(x = 0, y = y_ticks),
        ggplot2::aes(x = x, y = y, label = y),
        hjust = 1.5 + tick_len_prop * 50,
        size = tick_text_size, color = text_col
      )
  }

  # Title and custom axis labels with optional borders
  plot <- plot + ggplot2::labs(title = params$labels$title, y = NULL)

  x_adj <- params$labels$x_adjust; if (length(x_adj) != 2) x_adj <- c(0,0)
  y_adj <- params$labels$y_adjust; if (length(y_adj) != 2) y_adj <- c(0,0)

  x_lab_x <- max(x_range) + x_expand + as.numeric(x_adj[1])
  x_lab_y <- -0.05 * diff(y_range) + as.numeric(x_adj[2])
  y_lab_x <- as.numeric(y_adj[1])
  y_lab_y <- max(y_range) + 0.12 * max(y_range) + y_expand + as.numeric(y_adj[2])

  if (isTRUE(params$labels$show_borders)) {
    plot <- plot +
      ggplot2::annotate(
        "label",
        x = x_lab_x, y = x_lab_y,
        label = params$labels$x_label, size = params$labels$x_size,
        label.padding = grid::unit(0.3, "lines"),
        color = style_colors$text_field_color, fill = style_colors$text_field_fill
      ) +
      ggplot2::annotate(
        "label",
        x = y_lab_x, y = y_lab_y,
        label = params$labels$y_label, size = params$labels$y_size,
        label.padding = grid::unit(0.3, "lines"),
        color = style_colors$text_field_color, fill = style_colors$text_field_fill,
        angle = ifelse(isTRUE(params$labels$rotate_y), 90, 0)
      )
  } else {
    plot <- plot +
      ggplot2::annotate(
        "text",
        x = x_lab_x, y = x_lab_y,
        label = params$labels$x_label, size = params$labels$x_size, color = text_col
      ) +
      ggplot2::annotate(
        "text",
        x = y_lab_x, y = y_lab_y,
        label = params$labels$y_label, size = params$labels$y_size, color = text_col,
        angle = ifelse(isTRUE(params$labels$rotate_y), 90, 0)
      )
  }

  return(plot)
}

# Export Functions ----

#' Export plot to file
#' @noRd
.export_plot <- function(plot, export_options, verbose) {
  
  # Setup file path
  if (!is.null(export_options$path)) {
    if (!dir.exists(export_options$path)) {
      stop("Export path does not exist: ", export_options$path, call. = FALSE)
    }
    file_path <- file.path(export_options$path, paste0(export_options$filename, ".", export_options$format))
  } else {
    file_path <- paste0(export_options$filename, ".", export_options$format)
  }
  
  if (verbose) message("Exporting plot to: ", file_path)
  
  tryCatch({
    ggplot2::ggsave(
      filename = file_path,
      plot = plot,
      width = export_options$width,
      height = export_options$height,
      dpi = export_options$dpi,
      device = export_options$format
    )
    
    if (verbose) message("Plot exported successfully")
  }, error = function(e) {
    stop("Failed to export plot: ", e$message, call. = FALSE)
  })
}