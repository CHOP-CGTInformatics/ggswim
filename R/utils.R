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
get_layer_data <- function(data, mapping, i = 1L) {

  # Starting with color/colour, since that will always need to be given to result in a legend layer for ggsegment
  aes_mapping <- unlist(mapping)

  # TODO: Currently functionality is limited to and requires a color or fill aesthetic
  if (any(c("color", "colour") %in% names(aes_mapping))) {
    colour_or_color <- ifelse("colour" %in% names(aes_mapping), "colour", "color")
    colour_mapping <- data[[aes_mapping[[colour_or_color]] |> get_expr()]]
  } else {
    colour_or_color <- NULL
    colour_mapping <- NULL
  }

  if ("fill" %in% names(aes_mapping)) {
    fill_mapping <- data[[aes_mapping[["fill"]] |> get_expr()]]
  } else {
    fill_mapping <- NULL
  }

  if (!is.null(colour_mapping)) {
    # TODO: Unsure we can ever guarantee this is correct... but might be a reasonable assumption
    layer_data <- cbind(layer_data(i = i), colour_mapping) |>
      arrange(colour_mapping) # Assume correct since ggplot legend is arranged this way
  }

  if (!is.null(fill_mapping)) {
    # TODO: Unsure we can ever guarantee this is correct... but might be a reasonable assumption
    layer_data <- cbind(layer_data(i = i), fill_mapping) |>
      arrange(fill_mapping) # Assume correct since ggplot legend is arranged this way
  }

  layer_data
}
