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
                             size.unit = "mm",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Set up marker_key object containing information for the legend setup
  # and color overwriting in the setup_data portion of GeomSwimMarker
  marker_labels <- rlang::eval_tidy(data = data, expr = mapping$label)

  marker_key <- data.frame(
    marker_labels = marker_labels,
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
      glyph = glyph,
      na.rm = na.rm,
      ...
    )
  )

  # Reclass the layer to trigger ggplot_add and apply manual scale/guide
  class(layer_obj) <- c("swim_marker", class(layer_obj))
  layer_obj$marker_key <- marker_key
  layer_obj
}

#' @export
ggplot_add.swim_marker <- function(object, plot, object_name) {
  marker_key <- object$marker_key |>
    # TODO: Check if standard, are labels always in alphabetical order in legend display
    # Here required to correctly map guide colors to labels
    arrange(marker_labels)

  plot$layers <- append(plot$layers, object)

  # Add manual color/guide changes to the scale
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
                            # Expose expected color data from the marker_key
                            data <- left_join(data, params$marker_key, by = c("label" = "marker_labels"))
                            data
                          },

                          draw_panel = function(self, data, panel_params, coord, size.unit = "mm",
                                                na.rm = FALSE, marker_key = NULL, glyph = NULL) {
                            # Overwrite lost color data with marker_key color data
                            data <- data |>
                              mutate(
                                label = label_glyph
                              ) |>
                              select(-c(starts_with("marker_")))

                            GeomMarquee$draw_panel(data, panel_params, coord,
                                                   size.unit = size.unit, na.rm = na.rm)
                          }
)
