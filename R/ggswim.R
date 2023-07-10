#' @title Plot swimmer data
#'
#' @description
#' Plot swimmer data.
#'
#' @details
#' Lengths of objects supplied to the following parameters
#' must be equal to how many exist in the `df` supplied:
#' - `markers`
#' - `lane_colors`
#'
#' Note: `lane_colors` must have the same number of elements as `lanes`, even if
#' not all `lanes` are present in `df`.
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time the x-axis variable of the swimmer plot, typically a
#' function of time
#' @param events the column that will supply definitions for the `reference_event`,
#' `markers`, and `lanes`
#' @param reference_event a character string found in `events` that establishes
#' the time-zero reference point for the x-axis
#' @param emoji_or_shape One of "emoji" or "shape", determines whether to use
#' `geom_label()` or `geom_point()`
#' @param markers A character vector that will comprise point markers on the
#' swimmer plot. Optional, default `NULL`
#' @param lanes Columns that indicate line changes, i.e. color changes
#' for individual swim lanes.
#' @param lane_colors a vector of character strings to assign to the `lanes`,
#' written in the same order. Optional, if left `NULL`, default colors applied
#' @param title the plot title
#' @param xlab the x-axis label
#' @param ylab the y-axis label
#' @param legend_title the title of the legend
#'
#' @returns a ggplot2 figure
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw scale_x_continuous
#' labs guides theme guide_legend scale_color_manual element_text element_blank
#' element_rect geom_label theme_minimal ggplot_build
#' @importFrom cli cli_abort
#'
#' @export

ggswim <- function(df,
                   id,
                   time,
                   events,
                   reference_event,
                   emoji_or_shape,
                   markers,
                   lanes,
                   lane_colors = NULL,
                   title = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   legend_title = NULL) {

  # Streamline the dataframe ---------------------------------------------------
  # Capture variables as expressions, allowing for piping in API ---------------
  variables <- c("id", "time", "events", "reference_event")


  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  swim_tbl <- streamline(df = df,
                         id = id,
                         time = time,
                         events = events,
                         reference_event = reference_event,
                         markers = markers,
                         lanes = lanes)
  # check inputs ---------------------------------------------------------------

  # assign common vars ---------------------------------------------------------
  df <- swim_tbl$data
  markers <- swim_tbl$markers
  id <- swim_tbl$id
  time <- swim_tbl$time
  lanes <- swim_tbl$lanes
  if (!is.null(lane_colors)) {
    names(lane_colors) <- lanes
  }

  # Define initial gg object and apply lines colored by lanes ------------------
  gg <- df |>
    ggplot(aes(x = !!time, y = !!id, group = !!id)) +
    geom_line(aes(col = swim_tbl$data$lane_col),
              linewidth = 1.8)

  if (emoji_or_shape == "emoji") {

    gg <- gg +
      geom_label(
        aes(
          label = markers[marker_col], # nolint: object_usage_linter
          col = marker_col # nolint: object_usage_linter
        ),
        label.size = NA, fill = NA, na.rm = TRUE)

    guide_values <- get_guide_values(gg = gg, lanes = lanes, markers = markers)

     gg <- gg +
      guides(
        color = guide_legend(
          override.aes = list(
            linetype = guide_values$linetype_override,
            label = guide_values$label_override
          )
        )
      )
  }

  if (emoji_or_shape == "shape") {

    gg <- gg +
      geom_point(aes(
        shape = markers[marker_col], # nolint: object_usage_linter
        col = marker_col, # nolint: object_usage_linter
      ), size = 3, stroke = 1.5)

    guide_values <- get_guide_values(gg = gg, lanes = lanes, markers = markers)

    # TODO: Fix ordering for shapes
    gg <- gg +
      guides(color = guide_legend(
        override.aes = list(
          shape = guide_values$shape_override,
          stroke = guide_values$stroke_override,
          linetype = guide_values$linetype_override
        )
      )
      )
  }

  # Process and assign line colors from `lane_colors` for `scale_color_manual`
  assigned_line_colors <- get_assigned_line_colors(gg, lane_colors)

  gg +
    theme_minimal() +
    labs(x = xlab, y = ylab, title = title) +
    scale_color_manual(values = assigned_line_colors, name = legend_title)

  # TODO: Address label legend out of order, colors not taken into account, legend name, change overrides to function
}


#' @title apply an arbitrary number of `geom_point()`'s to a ggplot
#'
#' @description This function applies any number of necessary `geom_point()`
#' objects to a `ggplot2` chain as declared in `markers`.
#'
#' @return a ggplot object
#'
#' @param plot a ggplot2 object
#' @param markers a vector of columns that will comprise point markers on the
#' swimmer plot
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#'
#' @importFrom ggplot2 layer
#'
#' @keywords internal

apply_geom_points <- function(plot, markers, id) {
  # Determine the number of geom_point commands based on the length of markers
  num_geoms <- length(markers)

  # Create a list of geom_point layers
  layers <- lapply(seq_len(num_geoms), function(i) {
    layer(data = NULL, geom = "point",
          stat = "identity",
          position = "identity",
          mapping = aes(x = !!rlang::sym(markers[[i]]),
                        y = paste0(!!id),
                        col = paste(markers[[i]])))
  })

  # Add all layers to the existing plot using Reduce and the + operator
  plot <- Reduce(`+`, layers, init = plot)

  # Return the final plot with all layers applied
  return(plot)
}

#' @title Create guide values for legend
#'
#' @description
#' Programmatically assign and retrieve line and shape/label values to supply to
#' `ggplot2::guide()`.
#'
#' @param gg A `ggplot` object
#' @param emoji_or_shape One of "emoji" or "shape", determines whether to use
#' `geom_label()` or `geom_point()`
#' @param markers A character vector that will comprise point markers on the
#' swimmer plot. Optional, default `NULL`
#' @param lanes Columns that indicate line changes, i.e. color changes
#' for individual swim lanes.
#'
#' @returns a variable
#'
#' @importFrom ggplot2 ggplot_build
#'
#' @keywords internal

get_guide_values <- function(gg, emoji_or_shape, lanes, markers) {

  out <- list()

  # Label reorganization and identification
  # First, get labels as they appear in the ggplot object
  gg_obj <- ggplot_build(gg)
  legend_label_order <- gg_obj$plot$scales$scales[[1]]$get_labels()

  # Next, define the override for `guides()` later
  # Combine lanes and markers
  out$label_override <- c(
    lanes, markers
  )

  # Reorganize and subset for only what appears in the data set
  # This takes care of cases where not all lanes appear in the data
  names(out$label_override)[seq_along(lanes)] <- as.character(lanes)
  out$label_override[seq_along(lanes)] <- ""
  out$label_override <- out$label_override[names(out$label_override) %in% df$event]
  # Reorder based on legend_label_order, otherwise assigments will be mismatched
  out$label_override <- out$label_override[match(legend_label_order, names(out$label_override))]

  out$linetype_override <- ifelse(out$label_override == "", 1, 0) # 0 for blank, 1 for solid
  out$stroke_override <- ifelse(out$label_override == "", 1, 2) # values dictate stroke thickness
  out$shape_override <- out$label_override
  out$shape_override <- ifelse(out$shape_override == "", 32, as.integer(out$label_override)) # 32 for blank

  out
}

#' @title Reshape lane_colors
#'
#' @description
#' In order to assign the correct colors to `ggplot2::scale_color_manual()`,
#' it is necessary to account for the order of the legend output in combination
#' with the assigned `markers`.
#'
#' @returns a vector
#'
#' @param gg a `ggplot` object
#' @param lane_colors a vector of character strings to assign to the `lanes`,
#' written in the same order. Optional, if left `NULL`, default colors applied
#'
#' @keywords internal

get_assigned_line_colors <- function(gg, lane_colors) {

# Label reorganization and identification
# First, get labels as they appear in the ggplot object
gg_obj <- ggplot_build(gg)
legend_label_order <- gg_obj$plot$scales$scales[[1]]$get_labels()

# get only values that appear in the data
lines_to_keep <- unique(df$event[df$event %in% lanes])
colors <- lane_colors[match(lines_to_keep, names(lane_colors))]

# Combine the the `colors` and `legend_label_order` vectors and fill in missing
# values with NA
combined_vector <- rep(NA, length(legend_label_order))
names(combined_vector) <- legend_label_order
combined_vector[names(colors)] <- colors

combined_vector
}
