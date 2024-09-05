#' @title Add label markers to swimmer plots
#'
#' @description
#' Markers are specific symbols or indicators placed on the lanes of a swimmer plot
#' to denote particular events, milestones, or statuses. They provide additional
#' contextual information about significant occurrences during the timeline, such
#' as treatment responses or adverse events.
#'
#' @inheritParams ggplot2::geom_label
#'
#' @section Aesthetics:
#' [geom_swim_label()] understands the following aesthetics (required aesthetics are in bold)
#' when using `label_vals`/`label_names` similar to [geom_label()]. See "Notes" below for
#' additional considerations and requirements.
#'
#' - **`x`**
#' - **`y`**
#' - **label_vals**
#' - **`label_names`** *
#' - `alpha`
#' - `angle`
#' - `family`
#' - `fontface`
#' - `group`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#'
#' @section Notes:
#' - If using labels, both `label_vals` and `label_names` are required for
#' proper legend population. At minimum, `label_vals` is needed for data
#' display. These are unique parameter options for [aes()] to ggswim.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot() +
#'   geom_swim_label(
#'     data = end_study_events,
#'     mapping = aes(
#'       x = time_from_initial_infusion, y = pt_id,
#'       label_vals = end_study_label, label_names = end_study_name
#'     ),
#'     size = 5, label.size = NA
#'   )
#' }
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
  # Set default mapping if not provided and inherit.aes is TRUE
  if (is.null(mapping) && inherit.aes) {
    mapping <- aes()
  }

  # Modify the mapping to match expected aesthetics
  mapping$label <- mapping$label_vals
  mapping$colour <- mapping$label_names

  layer_obj <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )

  # Add custom attribute and modify class
  attr(layer_obj, "swim_class") <- "swim_label"
  class(layer_obj) <- c("swim_label", class(layer_obj))

  layer_obj
}

#' @export
ggplot_add.swim_label <- function(object, plot, object_name) {
  # Combine object and plot mappings; plot mapping takes precedence if both exist
  mapping <- modifyList(plot$mapping, object$mapping, keep.null = TRUE)
  mapping$label <- mapping$label_vals
  mapping$colour <- mapping$label_names

  # Ensure the combined mapping is set in the object
  object$mapping <- mapping

  # Enforce checks on the combined mapping
  check_supported_mapping_aes(
    mapping = object$mapping,
    unsupported_aes = "fill",
    parent_func = "geom_swim_label()"
  )

  # Append the object layer to the plot
  plot$layers <- append(plot$layers, object)

  # Fix legend handling
  label_override <- get_label_override(plot, object)

  plot <- plot +
    guides(
      colour = guide_legend(
        override.aes = list(
          label = label_override
        )
      )
    )

  # Add custom class if not already set
  if (!"ggswim_obj" %in% class(plot)) {
    class(plot) <- c("ggswim_obj", class(plot))
  }

  plot
}

#' @rdname geom_swim_label
#' @format NULL
#' @usage NULL
#' @export
GeomSwimLabel <- ggproto("GeomSwimLabel", GeomLabel,
  required_aes = c("x", "y", "label_vals", "label_names"),
  optional_aes = c("label"),
  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
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
#' @keywords internal

get_label_override <- function(plot, layer) {
  g <- try_ggswim(ggplot_build(plot))

  current_scale <- length(g$plot$scales$scales)

  label_layer_values <- g$plot$scales$scales[[current_scale]]$get_labels()

  # In case where labels are not defined in this layer, grab from top-level data
  if (is_empty(label_layer_values)) {
    label_layer_values <- g$plot$scales$scales[[1]]$get_labels()
  }

  # In case where data not assigned at this layer, grab from top-level data
  layer_data <- if (is_empty(layer$data)) {
    plot$data
  } else {
    layer$data
  }

  # In case where color/label defined in ggplot(), grab from top-level data

  if (is.null(layer$mapping$label_vals)) {
    layer$mapping$label_vals <- plot$mapping$label_vals
  }

  if (is.null(layer$mapping$label_names)) {
    layer$mapping$label_names <- plot$mapping$label_names
  }

  original_colour_var <- retrieve_original_aes(layer_data, aes_mapping = unlist(layer$mapping), aes_var = "colour")
  original_label_var <- retrieve_original_aes(layer_data, aes_mapping = unlist(layer$mapping), aes_var = "label")

  out <- tibble(
    label_values = label_layer_values
  ) |>
    left_join(
      layer_data |> select(all_of(original_colour_var), all_of(original_label_var)) |> unique(),
      by = c(label_values = original_colour_var)
    )

  out[original_label_var]
}
