#' @title Plot individual level response trajectories
#'
#' @description
#' Visualize individual record response trajectories over time using a swimmer plot.
#'
#' @details
#' A swimmer plot is a data visualization used to display individual
#' subject data over time. It shows events or outcomes as points along a
#' horizontal line for each subject, allowing easy comparison and pattern
#' identification.
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time the x-axis variable of the swimmer plot, typically a
#' function of time given in date or numeric format
#' @param events the column that will supply data for the `reference_event`,
#' `markers`, and `lanes` arguments
#' @param reference_event a character string found in `events` that establishes
#' the time-zero reference point for the x-axis for use when `time` is given in
#' a date/date-time format rather than in an integer/numeric format
#' @param markers a named list defining marker events on a `lane` in either
#' standard numeric ggplot 2 shapes, emoji, or unicode form (ex: "\U1F464").
#' Shapes can be supplied as character strings or integers.
#' @param shape_colors If providing shapes for `markers`, provide vector
#' specification for the colors those shapes should be. Default `NULL` for
#' non-shapes, default `ggplot2` colors if shapes with no colors specified.
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param groups additional specifier to indicate groups, optional. Example:
#' treatment groups or cohorts in a study.
#' @param legend_title the titles of the legends, given as a vector of character
#' strings
#'
#' @returns a ggplot2 figure
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_point
#' guides theme guide_legend scale_color_manual scale_fill_manual
#' geom_label ggplot_build labs
#'
#' @export

ggswim <- function(df,
                   id,
                   time,
                   events,
                   reference_event,
                   markers,
                   shape_colors = NULL,
                   lanes,
                   groups = NULL,
                   legend_title = NULL) {

  # Streamline the dataframe ---------------------------------------------------
  # Capture variables as expressions, allowing for piping in API ---------------
  variables <- c("id", "time", "events", "reference_event", "groups")

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
                         groups = groups,
                         lanes = lanes)
  # check inputs ---------------------------------------------------------------

  # assign common vars ---------------------------------------------------------
  df <- swim_tbl$data
  markers <- swim_tbl$markers
  id <- swim_tbl$id
  time <- swim_tbl$time
  lanes <- swim_tbl$lanes
  group_col <- swim_tbl$group_col # nolint: object_usage_linter
  group_vals <- swim_tbl$group_vals
  lane_colors <- get_lane_colors(lanes = swim_tbl$lanes,
                                 lane_colors = swim_tbl$lane_colors)

  # Determine whether the markers supplied are shape designations or emojis
  # Unicode and pasted emojis register as character, shapes should always be
  # numeric or numeric coercible. Suppress warning for NA coercions
  markers_numeric <- all(!is.na(suppressWarnings(as.numeric(unlist(markers)))))

  emoji_or_shape <- ifelse(
    markers_numeric,
    "shape",
    "emoji")

  # Define initial gg object and apply lines colored by lanes spec -------------
  gg <- df |>
    ggplot(aes(x = tdiff, y = !!id, group = !!id)) + # nolint: object_usage_linter
    geom_bar(aes(color = !!groups, fill = lane_col), stat = "identity", size = 1, width = .05) # nolint: object_usage_linter

  # Emoji Marker Handling ------------------------------------------------------
  # If markers supplied as emojis, apply geom_label()
  if (emoji_or_shape == "emoji") {

    gg <- gg +
      geom_label(
        aes(
          x = !!time,
          label = markers[marker_col], # nolint: object_usage_linter
          color = tidyr::fill(data = df, marker_col, .direction = "downup")$marker_col # nolint: object_usage_linter
        ),
        label.size = NA, fill = NA, na.rm = TRUE)
  }

  # Shape Marker Handling ------------------------------------------------------
  # If markers supplied as shape numerics, apply geom_point()
  if (emoji_or_shape == "shape") {

    gg <- gg +
      geom_point(aes(
        x = !!time,
        shape = markers[marker_col], # nolint: object_usage_linter
        color = tidyr::fill(data = df, marker_col, .direction = "downup")$marker_col, # nolint: object_usage_linter
      ), size = 5, stroke = 2, na.rm = TRUE)
  }

  # Update Legend Guide and Order ----------------------------------------------
  gg <- apply_gg_legend_order(gg, lanes, markers, groups = group_vals)

  guide_values <- get_guide_values(df = df,
                                   gg = gg,
                                   emoji_or_shape = emoji_or_shape,
                                   lanes = lanes,
                                   markers = markers,
                                   groups = groups,
                                   events = events,
                                   group_vals = group_vals)

  gg <- gg +
    if (emoji_or_shape == "emoji") {
      guides(
        color = guide_legend(
          override.aes = list(
            label = guide_values$label_override,
            linetype = guide_values$linetype_override,
            fill = rep(NA, length(guide_values$label_override))
          )
        ),
        fill = guide_legend(
          override.aes = list(
            label = guide_values$fill_override
          )
        )
      )
    } else {
      guides(
        color = guide_legend(
          override.aes = list(
            shape = guide_values$shape_override,
            stroke = guide_values$stroke_override,
            linetype = guide_values$linetype_override,
            fill = rep(NA, length(guide_values$label_override))
          )
        )
      )
    }


  # Process and assign line colors from `lane_colors` for `scale_color_manual`
  assigned_colors <- get_assigned_colors(df, gg, lanes, lane_colors, markers, shape_colors)

  # Suppress message related to existing color scale replacement
  suppressMessages(gg <- gg +
                     scale_fill_manual(values = assigned_colors$fills,
                                       breaks = names(assigned_colors$fills),
                                       name = legend_title[[1]]) +
                     labs(colour = legend_title[[2]])
  )
  if (emoji_or_shape == "shape") {
    suppressMessages(
      gg <- gg +
        scale_color_manual(values = assigned_colors$colors,
                           breaks = names(assigned_colors$colors),
                           name = legend_title[[1]])
    )
  }

  gg
}

#' @title Create guide values for legend
#'
#' @description
#' This function programmatically assigns override values for `guide()`.
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param gg A `ggplot` object
#' @param emoji_or_shape One of "emoji" or "shape", determines whether to use
#' `geom_label()` or `geom_point()`
#' @param markers a named list defining marker events on a `lane` in either
#' numeric shape or emoji form
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param groups additional specifier to indicate groups, optional. Example:
#' treatment groups or cohorts in a study.
#'
#' @returns a list
#'
#' @importFrom ggplot2 ggplot_build
#'
#' @keywords internal

get_guide_values <- function(df, gg, emoji_or_shape, lanes, markers,
                             groups,
                             events,
                             group_vals) {

  out <- list()
  groups <- ifelse(is.null(groups), "", groups)

  # Label reorganization and identification
  # First, get labels as they appear in the ggplot object
  legend_label_order <- update_gg_legend_order(gg, lanes, markers, group_vals)

  out$color_override <- unlist(c(group_vals, markers))
  # Reorganize and subset for only what appears in the data set
  names(out$color_override)[seq_along(group_vals)] <- as.character(group_vals)
  out$color_override[seq_along(group_vals)] <- ""
  out$color_override <- out$color_override[names(out$color_override) %in% df[[events]] | names(out$color_override) %in% df[[groups]]] # nolint: line_length_linter
  # Reorder based on legend_label_order, otherwise assignments will be mismatched
  out$color_override <- out$color_override[match(legend_label_order$color_label_order, names(out$color_override))]
  out$label_override <- out$color_override

  out$fill_override <- as.character(lanes)
  # Reorganize and subset for only what appears in the data set
  names(out$fill_override) <- lanes
  out$fill_override[seq_along(lanes)] <- ""
  out$fill_override <- out$fill_override[names(out$fill_override) %in% df[[events]]]
  # Reorder based on legend_label_order, otherwise assignments will be mismatched
  out$fill_override <- out$fill_override[match(legend_label_order$fill_label_order, names(out$fill_override))]

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
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param lane_colors a character vector defined in the `swim_tbl` that assigns
#' user-defined colors to `lanes`
#' @param markers a named list defining marker events on a `lane` in either
#' standard numeric ggplot 2 shapes, emoji, or unicode form (ex: "\U1F464").
#' Shapes can be supplied as character strings or integers.
#' @param shape_colors If providing shapes for `markers`, provide vector
#' specification for the colors those shapes should be. Default `NULL` for
#' non-shapes, default `ggplot2` colors if shapes with no colors specified.
#'
#' @importFrom scales hue_pal
#'
#' @keywords internal

get_assigned_colors <- function(df, gg, lanes, lane_colors, markers, shape_colors) {

  # Label reorganization and identification
  # First, get labels as they appear in the ggplot object
  gg_build <- ggplot_build(gg)$plot$scales$scales
  # Find the index positions where "colour" or "fill" is present
  index_with_colour <- which(sapply(gg_build, function(x) "colour" %in% x$aesthetics))
  index_with_fill <- which(sapply(gg_build, function(x) "fill" %in% x$aesthetics))

  color_legend_labels <- gg_build[[index_with_colour]]$get_labels()
  fill_legend_labels <- gg_build[[index_with_fill]]$get_labels()

  # get only values that appear in the data
  colors <- unlist(markers)[match(color_legend_labels, names(markers))]

  # If shape_colors provided, use them, else assign default ggplot color pallete
  if (!is.null(shape_colors)) {
    colors_names <- names(colors)
    names(shape_colors) <- names(colors)
    colors <- shape_colors
  } else {
    colors_names <- names(colors)
    colors <- hue_pal()(length(colors))
    names(colors) <- colors_names
  }

  fills <- lane_colors[match(fill_legend_labels, names(lane_colors))]

  list(colors = colors, fills = fills)
}

#' @title Get lane colors
#'
#' @description
#' This internal function detects whether or not lane colors are supplied in a
#' `swim_tbl` and returns them with the appropriate `lanes` names. If no colors
#' are given (i.e. `NULL`), default ggplot2 colors are supplied via the `scales`
#' package.
#'
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param lane_colors a character vector defined in the `swim_tbl` that assigns
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
