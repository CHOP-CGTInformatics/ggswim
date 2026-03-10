#' @noRd
pal_arrows <- function(colours = NULL, fills = NULL, types = NULL, n_values = NULL) {
  n_values <- n_values %||% max(length(colours), length(fills), length(types))
  if (n_values == 0) n_values <- 1

  arrows <- vctrs::new_rcrd(
    list(
      colour = rep(colours %||% "black", length.out = n_values),
      fill   = rep(fills %||% NA_character_, length.out = n_values),
      type   = rep(types %||% "closed", length.out = n_values)
    ),
    class = "swim_arrow"
  )

  function(n) {
    if (n > n_values) {
      cli::cli_warn(
        "This palette can handle a maximum of {n_values} values. You have supplied {n}."
      )
    }
    arrows[seq_len(n)]
  }
}

#' @noRd
draw_key_swim_arrow <- function(data, params, size) {
  alpha <- data$alpha[1]
  if (length(alpha) == 0 || is.na(alpha)) alpha <- 1

  linewidth <- data$linewidth[1]
  if (length(linewidth) == 0 || is.na(linewidth)) linewidth <- 0.5

  linetype <- data$linetype[1]
  if (length(linetype) == 0 || is.na(linetype)) linetype <- 1

  col <- data$colour[1]
  if (length(col) == 0 || is.na(col)) col <- "black"

  fill <- data$fill[1]
  if (length(fill) == 0 || is.na(fill)) fill <- col

  type <- "closed"

  if ("arrow" %in% names(data) && length(data$arrow) > 0 &&
      !vctrs::vec_detect_missing(data$arrow)[1]) {
    col  <- vctrs::field(data$arrow, "colour")[1]
    fill <- vctrs::field(data$arrow, "fill")[1]
    type <- vctrs::field(data$arrow, "type")[1]

    if (is.na(fill)) fill <- col
  }

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
#' @param data A dataframe prepared for use with [geom_swim_arrow()]. Required.
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#' @param arrow_colour The colour of the arrow head
#' @param arrow_fill The fill colour of the arrow head
#' @param arrow_head_length A unit specifying the length of the arrow head
#' (from tip to base).
#' @param arrow_neck_length Value specifying neck length from end of segment
#' to arrow head base
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head
#' should be a closed triangle.
#'
#' @section Aesthetics:
#' [geom_swim_arrow()] understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`y`**
#' - **xend**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' [geom_swim_arrow()] is a wrapper for [geom_segment()] and can support much of the same
#' functionality.
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
#' geom_swim_arrow(
#'   data = arrow_data,
#'   mapping = aes(xend = end_time, y = pt_id),
#'   linewidth = .1,
#'   arrow_neck_length = 5,
#'   arrow_head_length = grid::unit(0.25, "inches"),
#'   arrow_colour = "slateblue",
#'   arrow_fill = "cyan"
#' )
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

#' @rdname geom_swim_arrow
#' @format NULL
#' @usage NULL
#' @export
GeomSwimArrow <- ggproto("GeomSwimArrow", GeomSegment,
                         required_aes = c("y", "xend"),
                         non_missing_aes = c("linetype", "linewidth"),
                         default_aes = aes(
                           colour = "black",
                           fill = NA,
                           linewidth = 0.5,
                           linetype = 1,
                           alpha = NA,
                           arrow = NA
                         ),
                         draw_key = draw_key_swim_arrow,

                         setup_data = function(data, params) {
                           arrow_neck_length <- params$arrow_neck_length

                           if (is.null(arrow_neck_length)) {
                             arrow_neck_length <- max(data$xend, na.rm = TRUE) * 0.15
                           }

                           data |>
                             dplyr::mutate(
                               x = xend,
                               xend = xend + arrow_neck_length
                             )
                         },

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

                           if ("arrow" %in% names(data) && !all(is.na(data$arrow))) {
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

                             # one layer can only pass one arrow object to GeomSegment$draw_panel
                             # so enforce a single type per layer
                             if (length(unique(arrow_types)) > 1) {
                               cli::cli_abort("geom_swim_arrow() currently supports only one arrow type per layer.")
                             }

                             arrow <- grid::arrow(
                               type = unique(arrow_types),
                               length = arrow_head_length
                             )
                             arrow.fill <- data$fill
                           } else {
                             data$colour <- arrow_colour
                             arrow <- grid::arrow(type = arrow_type, length = arrow_head_length)
                           }

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

#' @export
scale_arrow_discrete <- function(colours = NULL, fills = NULL, types = NULL, limits = NULL, ...) {
  n_values <- max(c(length(colours), length(fills), length(types), length(limits)))
  if (n_values == 0) {
    arrow_df <- data.frame()
  } else {
    arrow_df <- data.frame(
      colour = colours %||% rep("black", n_values),
      fill   = fills %||% rep(NA_character_, n_values),
      type   = types %||% rep("closed", n_values),
      label  = limits %||% paste0("val", seq_len(n_values))
    ) |>
      dplyr::distinct()
  }

  palette <- pal_arrows(
    colours = arrow_df$colour,
    fills   = arrow_df$fill,
    types   = arrow_df$type,
    n_values = nrow(arrow_df)
  )

  discrete_scale(
    aesthetics = "arrow",
    scale_name = "swim_arrow",
    palette = palette,
    limits = arrow_df$label,
    ...,
    na.translate = FALSE
  )
}

#' @export
format.swim_arrow <- function(x, ...) {
  colours <- vctrs::field(x, "colour")
  fills   <- vctrs::field(x, "fill")
  types   <- vctrs::field(x, "type")

  paste0("Arrow: colour=", colours, ", fill=", fills, ", type=", types)
}
