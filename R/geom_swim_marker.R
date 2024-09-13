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

browser()

  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimMarker,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )

  marker_key <- rlang::eval_tidy(data = data, expr = mapping$label)

  # Automatically add scale_color_manual if `markers_df` is provided
  color_mapping <- scale_color_manual(
    aesthetics = "label",
    values = setNames(markers_df$marker_glyph, markers_df$Marker),
    guide = guide_legend(override.aes = list(color = markers_df$marker_color))
  )

  # Combine the layer with the color scale adjustment
  list(layer_obj, color_mapping)
}

#' @rdname geom_swim_label
#' @format NULL
#' @usage NULL
#' @export
GeomSwimMarker <- ggproto("GeomSwimMarker", GeomText,
                          required_aes = c("x", "y", "label")
)
