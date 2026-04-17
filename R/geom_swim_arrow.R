#' @noRd
#
# Internal helper used to draw the arrow symbol in the legend.
#
# This function is called by ggplot/ggswim when an arrow layer contributes a legend
# key. It does not draw arrows in the plot panel itself; it only draws the
# small representative arrow shown inside the legend box.
draw_key_swim_arrow <- function(data, params, size) {
  # Extract alpha, line width, and line type from the legend key data.
  # Because legend key data can contain NA values, each is given a safe default.
  # In particular, alpha must not be passed through as NA, or grid::gpar()
  # will error.
  alpha <- data$alpha[1]
  if (length(alpha) == 0 || is.na(alpha)) alpha <- 1

  linewidth <- data$linewidth[1]
  if (length(linewidth) == 0 || is.na(linewidth)) linewidth <- 0.5

  linetype <- data$linetype[1]
  if (length(linetype) == 0 || is.na(linetype)) linetype <- 1

  # Start with fallback colour/fill values taken from standard aesthetics.
  # These are used when the custom `arrow` aesthetic is not present.
  col <- data$colour[1]
  if (length(col) == 0 || is.na(col)) col <- "black"

  fill <- data$fill[1]
  if (length(fill) == 0 || is.na(fill)) fill <- col

  # Default to a closed arrowhead unless the mapped arrow aesthetic supplies
  # something else.
  type <- "closed"

  # If the layer is using the custom `arrow` aesthetic, unpack the arrow record
  # and use its colour, fill, and type values instead of the fallback settings.
  # This is what allows scale_arrow_discrete() to control the legend glyph.
  if (
    "arrow" %in% names(data) &&
      length(data$arrow) > 0 &&
      !vctrs::vec_detect_missing(data$arrow)[1]
  ) {
    col <- vctrs::field(data$arrow, "colour")[1]
    fill <- vctrs::field(data$arrow, "fill")[1]
    type <- vctrs::field(data$arrow, "type")[1]
  }

  # For closed arrowheads, use the line colour as the fill if no explicit fill
  # was supplied. This helps the legend key visually match the plotted arrows.
  if (is.na(fill)) fill <- col

  # Draw a short horizontal segment centered in the legend key and attach an
  # arrowhead to the right end. Coordinates are given in npc units, so the
  # arrow is drawn relative to the legend box rather than to plot data.
  #
  # Graphical parameters are set with grid::gpar():
  # - col controls the line/outline colour
  # - fill controls the fill of a closed arrowhead
  # - lwd and lty match the layer's line width and type
  #
  # Transparency is applied by folding alpha into the colour/fill values with
  # scales::alpha(), rather than passing alpha directly to gpar().
  grid::segmentsGrob(
    x0 = grid::unit(0.15, "npc"),
    y0 = grid::unit(0.5, "npc"),
    x1 = grid::unit(0.85, "npc"),
    y1 = grid::unit(0.5, "npc"),
    gp = grid::gpar(
      col = scales::alpha(col, alpha),
      fill = scales::alpha(fill, alpha),
      lwd = linewidth * ggplot2::.pt,
      lty = linetype,
      lineend = "butt",
      linejoin = "round"
    ),
    arrow = grid::arrow(
      type = type,
      length = params$arrow_head_length %||% grid::unit(0.15, "inches")
    )
  )
}

#' @title Add arrows to swimmer plot lanes
#'
#' @description
#' Arrows attached to the end of swimmer plot lanes can be used to denote the
#' continuation of events such as ongoing treatment, implying that the activity
#' or status extends beyond the plotted period.
#'
#' @details
#' Please note that [geom_swim_arrow()] requires a `data` argument and does not
#' inherit data like other functions.
#'
#' [geom_swim_arrow()] supports two approaches for defining arrow appearance:
#'
#' - By mapping the `arrow` aesthetic and supplying values with
#'   [scale_arrow_discrete()].
#' - By supplying `arrow_colour`, `arrow_fill`, and `arrow_type` directly as
#'   fixed parameters.
#'
#' When the `arrow` aesthetic is mapped, arrow appearance is controlled by
#' [scale_arrow_discrete()] and takes precedence over fixed arrow parameters.
#'
#' [geom_swim_arrow()] also supports two approaches for defining arrow extent:
#'
#' - By mapping both `x` and `xend`, in which case the arrow neck is drawn from
#'   `x` to `xend`.
#' - By mapping only `xend`, in which case `xend` is treated as the swimmer lane
#'   endpoint and the arrow neck is extended to the right using
#'   `arrow_neck_length`.
#'
#' If `x` is mapped, `arrow_neck_length` is ignored.
#'
#' @param data A data frame prepared for use with [geom_swim_arrow()]. Required.
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either `"stack"` or
#'   `"identity"` depending on the use case. Default is `"identity"`.
#' @param arrow_colour The outline colour of the arrow segment and arrow head
#'   when using fixed arrow parameters.
#' @param arrow_fill The fill colour of the arrow head when using fixed arrow
#'   parameters and a closed arrow type.
#' @param arrow_head_length A grid unit specifying the length of the arrow head
#'   (from tip to base).
#' @param arrow_neck_length A numeric value specifying the neck length from the
#'   end of the segment to the base of the arrow head when `x` is not mapped.
#' @param arrow_type One of `"open"` or `"closed"` indicating whether the arrow
#'   head should be drawn as an open or closed triangle when using fixed arrow
#'   parameters.
#'
#' @section Aesthetics:
#' [geom_swim_arrow()] understands the following aesthetics (required aesthetics
#' are in bold):
#'
#' - `x`
#' - **`y`**
#' - **`xend`**
#' - `alpha`
#' - `arrow`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' The `arrow` aesthetic is used to map discrete arrow styles via
#' [scale_arrow_discrete()].
#'
#' @section Legend behaviour:
#' To display arrows as their own legend component, map a value to the `arrow`
#' aesthetic and add [scale_arrow_discrete()]. If a single legend entry is
#' desired, map a constant such as `arrow = "Continuation"`.
#'
#' @section Fixed versus scaled arrow styling:
#' If the `arrow` aesthetic is not mapped, arrow appearance can be set directly
#' with `arrow_colour`, `arrow_fill`, and `arrow_type`.
#'
#' If the `arrow` aesthetic is mapped, arrow appearance is instead determined by
#' [scale_arrow_discrete()], and the fixed arrow parameters are used only as a
#' fallback.
#'
#' @section Underlying geom:
#' [geom_swim_arrow()] is a wrapper for [ggplot2::geom_segment()] and supports
#' much of the same functionality.
#'
#' @examples
#' # Set up data for arrows
#' arrow_data <- patient_data |>
#'   dplyr::left_join(
#'     end_study_events |>
#'       dplyr::select(pt_id, label),
#'     by = "pt_id"
#'   ) |>
#'   dplyr::select(pt_id, end_time, label) |>
#'   dplyr::filter(.by = pt_id, end_time == max(end_time)) |>
#'   dplyr::filter(!is.na(label)) |>
#'   unique()
#'
#' # Parameter-driven arrow styling
#' geom_swim_arrow(
#'   data = arrow_data,
#'   mapping = aes(xend = end_time, y = pt_id),
#'   linewidth = 0.1,
#'   arrow_neck_length = 5,
#'   arrow_head_length = grid::unit(0.25, "inches"),
#'   arrow_colour = "slateblue",
#'   arrow_fill = "cyan",
#'   arrow_type = "closed"
#' )
#'
#' # Mapped start and end positions
#' geom_swim_arrow(
#'   data = arrow_data,
#'   mapping = aes(x = start_time, xend = end_time, y = pt_id),
#'   linewidth = 0.1,
#'   arrow_head_length = grid::unit(0.25, "inches"),
#'   arrow_colour = "slateblue",
#'   arrow_fill = "cyan",
#'   arrow_type = "closed"
#' )
#'
#' # Scale-driven arrow styling with a separate legend entry
#' ggplot2::ggplot() +
#'   geom_swim_arrow(
#'     data = arrow_data,
#'     mapping = ggplot2::aes(
#'       xend = end_time,
#'       y = pt_id,
#'       arrow = "Continuation"
#'     ),
#'     linewidth = 0.1,
#'     arrow_neck_length = 5,
#'     arrow_head_length = grid::unit(0.25, "inches"),
#'     show.legend = c(arrow = TRUE)
#'   ) +
#'   scale_arrow_discrete(
#'     limits = "Continuation",
#'     colours = "slateblue",
#'     fills = "cyan",
#'     types = "closed",
#'     name = NULL
#'   )
#'
#' @export
geom_swim_arrow <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            arrow_colour = "black",
                            arrow_head_length = unit(0.25, "inches"),
                            arrow_neck_length = NULL,
                            arrow_fill = NULL,
                            arrow_type = "closed",
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimArrow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      arrow.fill = arrow_fill,
      arrow_colour = arrow_colour,
      arrow_head_length = arrow_head_length,
      arrow_neck_length = arrow_neck_length,
      arrow_type = arrow_type,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @noRd
extract_arrow_aesthetics <- function(data) {
  list(
    colour = vapply(data$arrow, function(x) vctrs::field(x, "colour"), character(1)),
    fill   = vapply(data$arrow, function(x) vctrs::field(x, "fill"), character(1)),
    type   = vapply(data$arrow, function(x) vctrs::field(x, "type"), character(1))
  )
}

#' @rdname geom_swim_arrow
#' @format NULL
#' @usage NULL
#' @export
GeomSwimArrow <- ggproto("GeomSwimArrow", GeomSegment,
  required_aes = c("y", "xend"),
  non_missing_aes = c("linetype", "linewidth"),
  default_aes = aes(
    x = NA,
    colour = "black",
    fill = NA,
    linewidth = 0.5,
    linetype = 1,
    alpha = NA,
    arrow = NA
  ),
  draw_key = draw_key_swim_arrow,

  # setup_data() prepares the input data before anything is drawn.
  #
  # geom_swim_arrow() supports two coordinate modes:
  #
  # 1. Derived-neck mode:
  #    If x is not mapped, the supplied xend value marks the end of the swimmer
  #    lane, and the arrow itself extends to the right from that point.
  #    To make that happen, setup_data() converts each input row into a short
  #    horizontal segment:
  #
  #    - x is set to the original xend (the start of the arrow neck)
  #    - xend is moved to the right by arrow_neck_length
  #
  # 2. Mapped-neck mode:
  #    If x is mapped, the supplied x and xend values are used directly as the
  #    start and end of the arrow neck segment.
  #
  # This means the geom can either draw only the arrow "extension" beyond a
  # swimmer lane or draw an explicitly positioned arrow segment.
  # If arrow_neck_length is not supplied in derived-neck mode, a default
  # proportional value is used based on the maximum observed xend.
  setup_data = function(data, params) {
    has_mapped_x <- "x" %in% names(data) && any(!is.na(data$x))

    if (has_mapped_x) {
      data
    } else {
      arrow_neck_length <- params$arrow_neck_length

      if (is.null(arrow_neck_length)) {
        arrow_neck_length <- max(data$xend, na.rm = TRUE) * 0.15
      }

      data |>
        dplyr::mutate(
          x = .data$xend,
          xend = .data$xend + arrow_neck_length
        )
    }
  },

  # draw_panel() is responsible for drawing the arrow segments in the plot
  # panel after setup_data() has created the neck segment coordinates.
  #
  # There are two styling modes:
  #
  # 1. Scale-driven mode:
  #    If the custom `arrow` aesthetic is mapped, arrow colour, fill, and head
  #    type are unpacked from that aesthetic. This is what allows
  #    scale_arrow_discrete() to control the plotted arrows and their legend.
  #
  # 2. Parameter-driven fallback mode:
  #    If the `arrow` aesthetic is not mapped, the geom falls back to the fixed
  #    parameters arrow_colour, arrow_fill, and arrow_type.
  #
  # Once styling is determined, draw_panel() delegates the actual drawing to
  # GeomSegment$draw_panel(), supplying a grid::arrow() object so that the short
  # segment created in setup_data() is rendered with an arrowhead attached.
  draw_panel = function(self, data, panel_params, coord,
                        arrow = NULL,
                        arrow.fill = NULL,
                        arrow_head_length = grid::unit(0.25, "inches"),
                        arrow_neck_length = NULL,
                        arrow_type = "closed",
                        arrow_colour = "black",
                        lineend = "butt",
                        linejoin = "round",
                        na.rm = FALSE) {
    # If a mapped `arrow` aesthetic is present, use it to define arrow styling.
    # Each arrow value is a record containing colour, fill, and type.
    if ("arrow" %in% names(data) && !all(vctrs::vec_detect_missing(data$arrow))) {
      data$colour <- vapply(
        data$arrow,
        function(x) vctrs::field(x, "colour"),
        character(1)
      )

      data$fill <- vapply(
        data$arrow,
        function(x) vctrs::field(x, "fill"),
        character(1)
      )

      arrow_types <- vapply(
        data$arrow,
        function(x) vctrs::field(x, "type"),
        character(1)
      )

      # grid::arrow() accepts a single arrow type for the draw call, so all
      # rows in a layer must currently share the same type.
      if (length(unique(arrow_types)) > 1) {
        cli::cli_abort(
          "geom_swim_arrow() currently supports only one arrow type per layer."
        )
      }

      arrow <- grid::arrow(
        type = unique(arrow_types),
        length = arrow_head_length
      )
      arrow.fill <- data$fill
    } else {
      # If no mapped `arrow` aesthetic is present, use the fixed styling
      # parameters supplied directly to geom_swim_arrow().
      data$colour <- arrow_colour
      arrow <- grid::arrow(
        type = arrow_type,
        length = arrow_head_length
      )
    }

    # Delegate final drawing to GeomSegment. At this point:
    # - setup_data() has already turned each row into a short horizontal segment
    #   or preserved mapped x/xend values
    # - this function has determined the arrow style
    # - GeomSegment draws the segment and attaches the arrowhead
    GeomSegment$draw_panel(
      data, panel_params, coord,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm
    )
  }
)
