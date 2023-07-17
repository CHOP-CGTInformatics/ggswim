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
#' the time-zero reference point for the x-axis for use when `time` is given in
#' a date/date-time format rather than in an integer/numeric format
#' @param emoji_or_shape One of "emoji" or "shape", determines whether to use
#' `geom_label()` or `geom_point()`
#' @param markers a named list defining marker events on a `lane` in either
#' numeric shape or emoji form
#' @param lanes a list of character strings that line segments for `id`. Colors
#' are also supplied here by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param legend_title the title of the legend
#'
#' @returns a ggplot2 figure
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw
#' guides theme guide_legend scale_color_manual
#' geom_label theme_minimal ggplot_build
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
                   legend_title = NULL) {

  # Streamline the dataframe ---------------------------------------------------
  # Capture variables as expressions, allowing for piping in API ---------------
  variables <- c("id", "time", "events", "reference_event")

  # Parse variables to be passed to streamline()
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
  lane_colors <- get_lane_colors(lanes = swim_tbl$lanes,
                                 lane_colors = swim_tbl$lane_colors)

  # Define initial gg object and apply lines colored by lanes spec -------------
  gg <- df |>
    ggplot(aes(x = !!time, y = !!id, group = !!id)) +
    geom_line(aes(col = swim_tbl$data$lane_col),
              linewidth = 1.8)

  # Emoji Marker Handling ------------------------------------------------------
  # If markers supplied as emojis, apply geom_label()
  if (emoji_or_shape == "emoji") {

    gg <- gg +
      geom_label(
        aes(
          label = markers[marker_col], # nolint: object_usage_linter
          col = marker_col # nolint: object_usage_linter
        ),
        label.size = NA, fill = NA, na.rm = TRUE)
  }

  # Shape Marker Handling ------------------------------------------------------
  # If markers supplied as shape numerics, apply geom_point()
  if (emoji_or_shape == "shape") {

    gg <- gg +
      geom_point(aes(
        shape = markers[marker_col], # nolint: object_usage_linter
        col = marker_col, # nolint: object_usage_linter
      ), size = 3, stroke = 1.5, na.rm = TRUE)
  }

  # Update Legend Guide and Order ----------------------------------------------
  gg <- apply_updated_legend_order(gg, lanes, markers)

  guide_values <- get_guide_values(df = df,
                                   gg = gg,
                                   emoji_or_shape = emoji_or_shape,
                                   lanes = lanes,
                                   markers = markers)

  gg <- gg +
    if (emoji_or_shape == "emoji") {
      guides(
        color = guide_legend(
          override.aes = list(
            linetype = guide_values$linetype_override,
            label = guide_values$label_override
          )
        )
      )
    } else {
      guides(
        color = guide_legend(
          override.aes = list(
            shape = guide_values$shape_override,
            stroke = guide_values$stroke_override,
            linetype = guide_values$linetype_override
          )
        )
      )
    }


  # Process and assign line colors from `lane_colors` for `scale_color_manual`
  assigned_line_colors <- get_assigned_line_colors(df, gg, lanes, lane_colors)

  gg <- gg +
    theme_minimal()

  # Supress message related to existing color scale replacement
  suppressMessages(gg <- gg +
                     scale_color_manual(values = assigned_line_colors,
                                        breaks = names(assigned_line_colors),
                                        name = legend_title)
  )

  gg
}

#' @title Create guide values for legend
#'
#' @description
#' Programmatically assign and retrieve all necessary override values to supply
#' to `ggplot2::guide()`.
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param gg A `ggplot` object
#' @param emoji_or_shape One of "emoji" or "shape", determines whether to use
#' `geom_label()` or `geom_point()`
#' @param markers a named list defining marker events on a `lane` in either
#' numeric shape or emoji form
#' @param lanes A list of character strings that line segments for `id`. Colors
#' are also supplied here by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#'
#' @returns a list
#'
#' @importFrom ggplot2 ggplot_build
#'
#' @keywords internal

get_guide_values <- function(df, gg, emoji_or_shape, lanes, markers) {

  out <- list()

  # Label reorganization and identification
  # First, get labels as they appear in the ggplot object
  gg_obj <- ggplot_build(gg)
  legend_label_order <- update_gg_legend_order(gg, lanes, markers)

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

  if (emoji_or_shape == "shape") {
    out$shape_override <- out$label_override
    out$shape_override <- ifelse(out$shape_override == "", 32, as.integer(out$label_override)) # 32 for blank
  }
  out
}

#' @title Reshape lane_colors
#'
#' @description
#' In order to assign the correct colors to `ggplot2::scale_color_manual()`,
#' it is necessary to account for the order of the legend output in combination
#' with the assigned `markers`. This way, only lanes that are present get
#' assigned the appropriate colors, in the order of the legend, and markers are
#' assigned `NA`.
#'
#' @returns a named vector
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param gg a `ggplot` object
#' @param lanes a list of character strings that line segments for `id`. Colors
#' are also supplied here by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param lane_colors a character vector defined in the `swim_tbl` that assigns
#' user-defined colors to `lanes`
#'
#' @keywords internal

get_assigned_line_colors <- function(df, gg, lanes, lane_colors) {

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

#' @title Get lane colors
#'
#' @description
#' This internal function detects whether or not lane colors are supplied in a
#' `swim_tbl` and returns them with the appropriate `lanes` names. If no colors
#' are given (i.e. `NULL`), default ggplot2 colors are supplied via the `scales`
#' package.
#'
#' @param lanes A list of character strings that line segments for `id`. Colors
#' are also supplied here by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' #' @param lane_colors a character vector defined in the `swim_tbl` that assigns
#' user-defined colors to `lanes`
#'
#' @returns A named vector
#'
#' @importFrom scales hue_pal

get_lane_colors <- function(lanes, lane_colors) {
  lane_colors <- lane_colors
  if (is.null(lane_colors)) {
    lane_colors <- hue_pal()(length(lanes))
    names(lane_colors) <- lanes
  }
  names(lane_colors) <- lanes
  lane_colors
}
