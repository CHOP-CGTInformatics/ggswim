#' @title ggswim title
#'
#' @description
#' A short description...
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param ...
#'
#' @export
#'
#' @importFrom ggplot2 aes ggplot geom_segment
#' @importFrom rlang get_expr
#' @importFrom dplyr arrange mutate row_number lag

ggswim_new <- function(
    data,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  out <- data |>
    ggplot() +
    geom_segment(
      mapping,
      ...
    )

  #TODO: Finalize
  # -------
  layer_data <- get_layer_data(data, mapping)

  class(out) <- c("ggswim_obj", class(out))

  # Define a new object to reference later, stashed in the ggplot object
  out$ggswim_overrides <- list()

  # TODO: Functionalize and finalize
  ignored_mapping_names <- c("x", "y", "yend", "xend")
  used_mapping_names <- names(mapping)[!names(mapping) %in% ignored_mapping_names]

  out$ggswim_overrides <- vector("list", length(used_mapping_names))
  names(out$ggswim_overrides) <- used_mapping_names

  # Testing mapping layer ----
  for (mapping in used_mapping_names) {
    out$ggswim_overrides[[mapping]] <- unique(layer_data[[mapping]])
  }

  # Return object
  out
}

#' @title Build Layer Data
#'
#' @description
#' A short description...
#'
#' @returns A dataframe
#'
#' @param data ...
#' @param mapping ...
#'
#' @keywords internal
#'
#' @importFrom ggplot2 layer_data
#' @importFrom rlang get_expr

get_layer_data <- function(data, mapping) {
  # TODO: Functionalize and finalize
  # Testing to get factor levels for legend layers ----
  # Starting with color/colour, since that will always need to be given to result in a legend layer for ggsegment
  aes_mapping <- unlist(mapping)
  colour_or_color <- ifelse("colour" %in% names(aes_mapping), "colour", "color")
  color_mapping <- data[[aes_mapping[[colour_or_color]] |> get_expr()]]
  y_mapping <- data[[aes_mapping[["y"]] |> get_expr()]] # Might just be a sanity check

  # Unsure we can ever guarantee this is correct... but might be a reasonable assumption
  #TODO: Address current issue where add_marker_new references previous layer, not current one
  layer_data <- cbind(ggplot2::layer_data(), color_mapping) |>
    cbind(y_mapping) |>
    arrange(color_mapping) # Assume correct since ggplot legend is arranged this way
}
