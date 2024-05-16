#' @title Add markers of interest swimmer plots - labels
#'
#' @inheritParams ggplot2::geom_label
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot() +
#' geom_swim_label(data = end_study_events,
#'                 mapping = aes(x = time_from_initial_infusion, y = pt_id,
#'                               label_vals = end_study_label, label_names = end_study_name),
#'                 size = 5, label.size = NA)

geom_swim_label <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
                            size.unit = "mm",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  structure(
    "A geom_swim_label layer.",
    class = "marker_label",
    stat = stat,
    position = position,
    mapping = mapping,
    data = data,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      size.unit = size.unit,
      ... = ...
    )
  )
}

#' @export
ggplot_add.marker_label <- function(object, plot, object_name) {
  # Retrieve object vals ---
  data <- attr(object, "data") # nolint object_usage_linter

  # Enforce checks ----
  mapping <- attr(object, "mapping")

  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_point()"
  )

  # Convert label mapping params to linked standard params for intuitive API
  names(mapping)[names(mapping) == "label_vals"] <- "label"
  names(mapping)[names(mapping) == "label_names"] <- "colour"

  new_layer <- layer(
    data = attr(object, "data"),
    mapping = mapping,
    stat = attr(object, "stat"),
    geom = GeomSwimLabel,
    position = attr(object, "position"),
    key_glyph = "label",
    show.legend = attr(object, "show.legend"),
    inherit.aes = attr(object, "inherit.aes"),
    params = attr(object, "params")
  )

  # Tag the layer with a reference attribute
  new_layer$swim_class <- "marker_label"

  plot$layers <- append(plot$layers, new_layer)

  # Fix legend ----
  label_override <- get_label_override(plot, new_layer)

  plot <- plot +
    guides(
      colour = guide_legend(
        override.aes = list(
          label = label_override
        )
      )
    )

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
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        size.unit = "mm") {
    # Return all components
    grid::gList(
      GeomLabel$draw_panel(data, panel_params, coord,
        parse = FALSE,
        na.rm = na.rm,
        label.padding = label.padding,
        label.r = label.r,
        label.size = label.size,
        size.unit = size.unit
      )
    )
  }
)

#' @title Fix Label Legend
#'
#' @description
#' Fixes the legend for labels, while accounting for use of [new_scale_colour()],
#' so that labels appear over legend glyphs.
#'
#' @param plot The current plot
#' @param layer The new label layer being added
#'
#' @returns
#' A dataframe, acting as a key for [guide_legend()] label aes overrides

get_label_override <- function(plot, layer) {
  g <- ggplot_build(plot)

  current_scale <- length(g$plot$scales$scales)

  label_layer_values <- g$plot$scales$scales[[current_scale]]$get_labels()

  original_colour_var <- retrieve_original_aes(layer$data, aes_mapping = unlist(layer$mapping), aes_var = "colour")
  original_label_var <- retrieve_original_aes(layer$data, aes_mapping = unlist(layer$mapping), aes_var = "label")

  out <- tibble(
    label_values = label_layer_values
  ) |>
    left_join(
      layer$data |> select(all_of(original_colour_var), all_of(original_label_var)) |> unique(),
      by = c(label_values = original_colour_var)
    )

  out[original_label_var]
}
