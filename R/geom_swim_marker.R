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
                             nudge_x = 0,
                             nudge_y = 0,
                             size.unit = "mm",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Copied from geom_text
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        "i" = "Only use one approach to alter the position."
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  marker_labels <- rlang::eval_tidy(data = data, expr = mapping$label)

  marker_key <- data.frame(
    marker_labels = marker_labels,
    label_marquee = paste0("{.", color, " ", marker_labels, "}"),
    marker_glyphs = glyph,
    label_glyph = paste0("{.", color, " ", glyph, "}"),
    marker_colors = color
  ) |> distinct()

  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimMarker,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      size.unit = size.unit,
      marker_key = marker_key,
      na.rm = na.rm,
      ...
    )
  )

  # Combine the layer with the color scale adjustment
  class(layer_obj) <- c("swim_marker", class(layer_obj))
  layer_obj$marker_key <- marker_key
  layer_obj
}

#' @export
ggplot_add.swim_marker <- function(object, plot, object_name) {
  # Combine object and plot mappings; plot mapping takes precedence if both exist

  marker_key <- object$marker_key |>
    # TODO: Check if standard, are labels always in alphabetical order in legend display
    # Here required to correctly map guide colors to labels
    arrange(marker_labels)

  plot$layers <- append(plot$layers, object)

  plot +
    scale_color_manual(
      aesthetics = "label",
      name = plot$layers[[length(plot$layers)]]$mapping$label |> rlang::as_label(),
      values = setNames(marker_key$marker_glyphs, marker_key$marker_labels),
      guide = guide_legend(override.aes = list(color = marker_key$marker_colors))
    )
}

#' @rdname geom_swim_marker
#' @format NULL
#' @usage NULL
#' @export
GeomSwimMarker <- ggproto("GeomSwimMarker", GeomMarquee,
                          required_aes = c("x", "y", "label"),

                          setup_data = function(data, params) {

                            data <- left_join(data, params$marker_key, by = c("label" = "marker_labels"))
                            data
                          },

                          draw_panel = function(self, data, panel_params, coord, size.unit = "mm",
                                                na.rm = FALSE, marker_key = NULL) {

                            data <- data |>
                              mutate(
                                label = label_glyph
                              ) |>
                              select(-c(starts_with("marker_")))

                            GeomMarquee$draw_panel(data, panel_params, coord,
                                                   size.unit = size.unit, na.rm = na.rm)
                          }
)
