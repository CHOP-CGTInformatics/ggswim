#' @title Add markers of interest to level response trajectories
#'
#' @description
#' "Markers" are used to specify events of interest along response trajectories
#' across individual lanes.
#'
#' @returns A ggswim object
#'
#' @param data a dataframe prepared for use with `ggswim()`, either coming from
#' a parent `ggswim()` function, another `add_marker()` call, or a new dataframe
#' prepared for use with `ggswim()`.
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ... Other arguments passed to `geom_point`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @export
#'
#' @importFrom ggplot2 aes geom_point geom_label

add_marker <- function(
    data = NULL,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  labels <- ifelse("label" %in% names(mapping), TRUE, FALSE)

  if (labels) {
    # mapping$colour <- mapping$label # force color mapping with labels

    out <- geom_label(
      data = data,
      mapping = mapping,
      ...
    )

    out$swim_class <- "marker_label"
  } else {
    out <- geom_point(
      data = data,
      mapping = mapping,
      ...
    )

    out$swim_class <- "marker_point"
  }

  out
}


#' @title Fix legend display for multiple layers
#'
#' @description
#' This function is primarily helpful when combining labels (like unicode emojis)
#' and point (like shapes). This function seeks to override the values and present
#' them in the same layer of the legend output.
#'
#' @details
#' In its current state, `fix_legend()` can only work with a pre-rendered ggswim
#' plot object, therefore it cannot be added to the `+` operator chain.
#'
#' @param ggswim_obj A ggswim object
#'
#' @importFrom rlang is_empty
#' @importFrom dplyr bind_rows select arrange
#' @importFrom tidyselect any_of
#' @importFrom ggplot2 guides guide_legend

fix_legend <- function(ggswim_obj) {

  # Determine indices of layers in ggplot object that contain labels and points
  label_layer_indices <- c()
  point_layer_indices <- c()

  label_layer_data <- data.frame()
  point_layer_data <- data.frame()
  override <- list(
    "colour" = NULL,
    "fill" = NULL,
    "shape" = NULL
  )

  for (i in seq_along(ggswim_obj$layers)) {
    if (ggswim_obj$layers[[i]]$swim_class == "marker_label") {
      label_layer_indices <- c(label_layer_indices, i)
    }

    if (ggswim_obj$layers[[i]]$swim_class == "marker_point") {
      point_layer_indices <- c(point_layer_indices, i)
    }
  }

  for (i in label_layer_indices) {
    if (is_empty(label_layer_data)) {
      label_layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                         mapping = ggswim_obj$layers[[i]]$mapping,
                                         i = i)
    } else {
      added_label_layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                               mapping = ggswim_obj$layers[[i]]$mapping,
                                               i = i)

      label_layer_data <- rbind(label_layer_data, added_label_layer_data)
    }
  }

  for (i in point_layer_indices) {
    if (is_empty(point_layer_data)) {
      point_layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                         mapping = ggswim_obj$layers[[i]]$mapping,
                                         i = i)
    } else {
      added_point_layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                               mapping = ggswim_obj$layers[[i]]$mapping,
                                               i = i)

      point_layer_data <- dplyr::bind_rows(point_layer_data, added_point_layer_data)
    }
  }

  accepted_colour_columns <- c(
    "colour", "label", "group", "fill", "size", "shape", "stroke", "colour_mapping"
  )

  override$colour <- bind_rows(label_layer_data, point_layer_data) |>
    select(any_of(accepted_colour_columns)) |>
    arrange(colour_mapping) |>
    unique()

  if ("label" %in% names(override$colour)) {
    override$colour$label[is.na(override$colour$label)] <- ""
  }

  override$shape <- "none" # TODO: May need to get rid of this

  ggswim_obj +
    guides(
      shape = override$shape,
      colour = guide_legend(
        override.aes = list(
          label = override$colour$label,
          fill = override$colour$fill,
          color = override$colour$colour,
          shape = override$colour$shape
        )
      )
    )
}
