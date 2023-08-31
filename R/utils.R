#' @title Build Layer Data
#'
#' @description
#' A short description...
#'
#' @returns A dataframe
#'
#' @param data ...
#' @param mapping ...
#' @param i An integer to supply for the layer to retrieve.
#' If none given, defaults to `1L`.
#'
#' @keywords internal
#'
#' @importFrom ggplot2 layer_data
#' @importFrom rlang get_expr

get_layer_data <- function(data, mapping, i = 1L) {

  # Starting with color/colour, since that will always need to be given to result in a legend layer for ggsegment
  aes_mapping <- unlist(mapping)

  #TODO: Currently functionality is limited to and requires a color aesthetic
  if (any(c("color", "colour") %in% names(aes_mapping))) {
    colour_or_color <- ifelse("colour" %in% names(aes_mapping), "colour", "color")
    color_mapping <- data[[aes_mapping[[colour_or_color]] |> get_expr()]]
  } else {
    colour_or_color <- NULL
    color_mapping <- NULL
  }

  y_mapping <- data[[aes_mapping[["y"]] |> get_expr()]] #TODO: Remove. Just a sanity check, shows mapping is equal to y col in layer_data output

  if (!is.null(color_mapping)) {
    #TODO: Unsure we can ever guarantee this is correct... but might be a reasonable assumption
    layer_data <- cbind(ggplot2::layer_data(i = i), color_mapping) |>
      cbind(y_mapping) |> #TODO: Remove
      arrange(color_mapping) # Assume correct since ggplot legend is arranged this way
  }
}

#' @title Insert override layer element
#'
#' @description
#' A short description...
#'
#' @returns A ggplot layer object appended with `overrides`
#'
#' @keywords internal
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param layer_obj a ggswim layer object
#' @param current_layer An integer value corresponding to the current working layer
#' @param mapping description
#' @param ignore_mapping A vector of characters for mapping values to ignore.
#' Will typically match "required" aes elements from ggplot geom functions

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

  # TODO: Functionalize and finalize
  used_mapping_names <- names(mapping)[!names(mapping) %in% ignore_mapping]

  out$overrides <- vector("list", length(used_mapping_names))
  names(out$overrides) <- used_mapping_names

  # Testing mapping layer ----
  for (mapping in used_mapping_names) {
    out$overrides[[mapping]] <- unique(layer_data[[mapping]])
  }

  out
}
