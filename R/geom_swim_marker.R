#' @title Add markers to swimmer plots
#'
#' @description
#' Markers are specific symbols or indicators placed on the lanes of a swimmer plot
#' to denote particular events, milestones, or statuses. They provide additional
#' contextual information about significant occurrences during the timeline, such
#' as treatment responses or adverse events.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @export

geom_swim_marker <- function(mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             ...,
                             color = NULL,
                             glyph = NULL,
                             parse = FALSE,
                             nudge_x = 0,
                             nudge_y = 0,
                             check_overlap = FALSE,
                             size.unit = "mm",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Copied from ggplot2::geom_text
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        "i" = "Only use one approach to alter the position."
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  # Set up marker_key object containing information for the legend setup
  # and color overwriting in the setup_data portion of GeomSwimMarker
  marker_labels <- rlang::eval_tidy(data = data, expr = mapping$label)

  marker_key <- data.frame(
    marker_labels = marker_labels,
    marker_glyphs = glyph,
    marker_colors = color
  ) |> distinct()

  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = StatMarker,
    geom = GeomSwimMarker,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      size.unit = size.unit,
      marker_key = marker_key,
      na.rm = na.rm,
      ...
    )
  )

  # Reclass the layer to trigger ggplot_add and apply manual scale/guide
  class(layer_obj) <- c("swim_marker", class(layer_obj))
  layer_obj$marker_key <- marker_key
  layer_obj
}

#' @rdname geom_swim_marker
#' @export
StatMarker <- ggplot2::ggproto(
  `_class` = "StatMarker",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y", "label"),
  compute_group = function(data, scales, marker_key, ...) {
    compute_marker_group(data = data, scales = scales, marker_key = marker_key)
  }
)

#' @rdname geom_swim_marker
#' @keywords internal
compute_marker_group <- function(data, scales, marker_key) {
  data <- left_join(data, marker_key, by = c("label" = "marker_labels"))
  data
}

#' @export
ggplot_add.swim_marker <- function(object, plot, object_name) {
  marker_key <- object$marker_key |>
    arrange(marker_labels)

  plot$layers <- append(plot$layers, object)

  # Add manual color/guide changes to the scale
  plot +
    scale_color_manual(
      aesthetics = "label",
      name = plot$layers[[length(plot$layers)]]$mapping$label |> rlang::as_label(),
      values = setNames(marker_key$marker_glyphs, marker_key$marker_labels),
      guide = guide_legend(
        override.aes = list(
          color = marker_key$marker_colors
        )
      )
    )
}

#' @rdname geom_swim_marker
#' @format NULL
#' @usage NULL
#' @export
GeomSwimMarker <- ggproto("GeomSwimMarker", GeomText,
                          required_aes = c("x", "y", "label"),
                          optional_aes = "colour",

                          draw_panel = function(self, data, panel_params, coord, size.unit = "mm",
                                                parse = FALSE, check_overlap = FALSE, na.rm = FALSE) {
                            # Note this must be carried out down here and not in the compute
                            # stat function. Doing so will result in broken connections for
                            # the label aesthetic
                            data <- data |>
                              mutate(
                                colour = marker_colours
                              ) |>
                              select(-c(starts_with("marker_")))

                            GeomText$draw_panel(data, panel_params, coord,
                                                parse = parse, check_overlap = check_overlap,
                                                size.unit = size.unit, na.rm = na.rm)
                          }
)
