#' @title Build Layer Data
#'
#' @description
#' This internal function looks for color and fill aesthetic mapping data in
#' constructed ggplot objects and appends layer data with associated data from
#' the parent data frame.
#'
#' @returns A dataframe
#'
#' @param data The data responsible for the current layer
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param i An integer to supply for the layer to retrieve. If none given, defaults
#' to `1L`.
#'
#' @keywords internal
#'
#' @importFrom ggplot2 layer_data
#' @importFrom rlang get_expr
#' @importFrom dplyr arrange

get_layer_data <- function(data, mapping, i = 1L) {

  # Starting with color/colour, since that will always need to be given to result in a legend layer for ggsegment
  aes_mapping <- unlist(mapping)

  #TODO: Currently functionality is limited to and requires a color or fill aesthetic
  if (any(c("color", "colour") %in% names(aes_mapping))) {
    colour_or_color <- ifelse("colour" %in% names(aes_mapping), "colour", "color")
    color_mapping <- data[[aes_mapping[[colour_or_color]] |> get_expr()]]
  } else {
    colour_or_color <- NULL
    color_mapping <- NULL
  }

  if ("fill" %in% names(aes_mapping)) {
    fill_mapping <- data[[aes_mapping[["fill"]] |> get_expr()]]
  } else {
    fill_mapping <- NULL
  }

  if (!is.null(color_mapping)) {
    #TODO: Unsure we can ever guarantee this is correct... but might be a reasonable assumption
    layer_data <- cbind(layer_data(i = i), color_mapping) |>
      arrange(color_mapping) # Assume correct since ggplot legend is arranged this way
  }

  if (!is.null(fill_mapping)) {
    #TODO: Unsure we can ever guarantee this is correct... but might be a reasonable assumption
    layer_data <- cbind(layer_data(i = i), fill_mapping) |>
      arrange(fill_mapping) # Assume correct since ggplot legend is arranged this way
  }

  layer_data
}

#' @title Insert override layer element
#'
#' @description
#' A helper function that wraps `get_layer_data` and adds the result to a ggplot
#' layer.
#'
#' @returns A ggplot layer object appended with the `overrides` list object
#'
#' @keywords internal
#'
#' @param data The data responsible for the current layer
#' @param layer_obj a ggplot layer object
#' @param current_layer An integer value corresponding to the current working layer
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ignore_mapping A vector of characters for mapping values to ignore.
#' Will typically match "required" `aes` elements from ggplot geom functions

insert_override <- function(
    data,
    layer_obj,
    current_layer,
    mapping,
    ignore_mapping
) {
  out <- layer_obj
  layer_data <- get_layer_data(data, mapping, i = current_layer)

  # Define a new object to reference later, stashed in the current layer
  out$overrides <- list()

  # Subset aesthetic definitions mapped that aren't required by the geom
  used_mapping_names <- setdiff(names(mapping), ignore_mapping)

  # Assign named list elements based on the used mapping elements
  out$overrides <- vector("list", length(used_mapping_names))
  names(out$overrides) <- used_mapping_names

  # Assign the corresponding values to those mapping elements
  for (map_element in used_mapping_names) {
    out$overrides[[map_element]] <- unique(layer_data[[map_element]])
  }

  out
}
