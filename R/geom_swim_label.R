#' @title Add markers of interest swimmer plots - labels
#'
#' @inheritParams ggplot2::geom_label
#'
#' @export
geom_swim_label <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
                            size.unit = "mm",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  env <- environment()
  env_list <- list(env)[[1]]

  structure(list(expr = env_list, dots = dots_list(...)), class = "marker_label")
}

#' @export
ggplot_add.marker_label <- function(object, plot, object_name) {
  list2env(as.list(object$expr), current_env())

  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_point()"
  )

  # Convert label mapping params to linked standard params for intuitive API
  names(mapping)[names(mapping) == "label_vals"] <- "label"
  names(mapping)[names(mapping) == "label_names"] <- "colour"

  params <- append(list(na.rm = na.rm,
                        parse = parse,
                        label.padding = label.padding,
                        label.r = label.r,
                        label.size = label.size,
                        size.unit = size.unit),
                   object$dots)

  new_layer <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimLabel,
    position = position,
    key_glyph = "label",
    show.legend = show.legend,
    inherit.aes = inherit.aes
    # params = list(na.rm = na.rm, ...)
    # params = params
  )

  # TODO: Figure out why this doesn't work in `layer()` above
  new_layer$geom_params <- params
  new_layer$stat_params <- params

  # Tag the layer with a reference attribute
  new_layer$swim_class <- "marker_label"

  plot$layers <- append(plot$layers, new_layer)

  # Return
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_label
#' @format NULL
#' @usage NULL
#' @export
GeomSwimLabel <- ggproto("GeomSwimLabel", Geom,
                         required_aes = c("x", "y", "label"),
                         default_aes = aes(
                           colour = NA, fill = NA, size = 3.88, angle = 0,
                           hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                           lineheight = 1.2
                         ),
                         draw_panel = function(data, panel_params, coord, optional_aes, label.size = NA, ...) {
                           # Return all components
                           grid::gList(
                             GeomLabel$draw_panel(data, panel_params, coord, label.size = label.size, ...)
                           )
                         }
)
