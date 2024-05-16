#' @title Add markers of interest swimmer plots - point
#' @export
geom_swim_point <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  structure(
    "A geom_swim_point layer.",
    class = "marker_point",
    stat = stat,
    position = position,
    mapping = mapping,
    data = data,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ... = ...
    )
  )
}

#' @export
ggplot_add.marker_point <- function(object, plot, object_name) {
  # Enforce checks ----
  mapping <- attr(object, "mapping")

  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_point()"
  )

  new_layer <- layer(
    data = attr(object, "data"),
    mapping = mapping,
    stat = attr(object, "stat"),
    geom = GeomSwimPoint,
    position = attr(object, "position"),
    show.legend = attr(object, "show.legend"),
    inherit.aes = attr(object, "inherit.aes"),
    params = attr(object, "params")
  )

  # Tag the layer with a reference attribute
  new_layer$swim_class <- "marker_point"

  plot$layers <- append(plot$layers, new_layer)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_point
#' @format NULL
#' @usage NULL
#' @export
GeomSwimPoint <- ggproto("GeomSwimPoint", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_panel = function(data, panel_params, coord, ...) {
    # Return all components
    grid::gList(
      GeomPoint$draw_panel(data, panel_params, coord, ...)
    )
  }
)
