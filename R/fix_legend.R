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
#' @returns A ggswim object
#'
#' @importFrom rlang is_empty
#' @importFrom dplyr bind_rows select arrange
#' @importFrom tidyselect any_of
#' @importFrom ggplot2 guides guide_legend

fix_legend <- function(ggswim_obj) {

  label_layer_indices <- c()
  point_layer_indices <- c()

  label_layer_data <- data.frame()
  point_layer_data <- data.frame()
  override <- list()

  # Determine indices of layers in ggplot object that contain labels and points
  for (i in seq_along(ggswim_obj$layers)) {
    if (ggswim_obj$layers[[i]]$swim_class == "marker_label") {
      label_layer_indices <- c(label_layer_indices, i)
    }

    if (ggswim_obj$layers[[i]]$swim_class == "marker_point") {
      point_layer_indices <- c(point_layer_indices, i)
    }
  }

  # Create bound layer dataframes
  label_layer_data <- bind_layer_data(ggswim_obj,
                                      layer_indices = label_layer_indices,
                                      layer_data = label_layer_data)

  point_layer_data <- bind_layer_data(ggswim_obj,
                                      layer_indices = point_layer_indices,
                                      layer_data = point_layer_data)

  # TODO: Verify column names
  accepted_colour_columns <- c(
    "colour", "label", "group", "fill", "size", "shape", "stroke", "colour_mapping"
  )

  # Define override aesthetic guides
  override$colour <- bind_rows(label_layer_data, point_layer_data) |>
    select(any_of(accepted_colour_columns)) |>
    arrange(colour_mapping) |>
    unique()

  if ("label" %in% names(override$colour)) {
    override$colour$label[is.na(override$colour$label)] <- ""
  }

  override$shape <- "none" # TODO: Determine if default should always be removal

  # Return fixed ggswim object
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


#' @title Bind layer dataframes for legend
#'
#' @description
#' Internal helper function that returns layer data from `get_layer_data()`
#' as a bound dataframe to help with legend guide definitions.
#'
#' @returns A dataframe
#'
#' @param ggswim_obj description
#' @param layer_indices description
#' @param layer_data description
#'
#' @keywords internal

bind_layer_data <- function(ggswim_obj, layer_indices, layer_data) {
  for (i in layer_indices) {
    # If first layer, overwrite empty variable
    if (is_empty(layer_data)) {
      layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                   mapping = ggswim_obj$layers[[i]]$mapping,
                                   i = i)
    } else {
      added_layer_data <- get_layer_data(data = ggswim_obj$layers[[i]]$data,
                                         mapping = ggswim_obj$layers[[i]]$mapping,
                                         i = i)

      layer_data <- bind_rows(layer_data, added_layer_data)
    }
  }

  layer_data
}
