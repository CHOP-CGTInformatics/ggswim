#' @title ggswim title
#'
#' @description
#' A short description...
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param mapping description
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

  # Define new class 'ggswim_obj``
  class(out) <- c("ggswim_obj", class(out))
  current_layer <- length(out$layers) # The max length can be considered the current working layer

  # Define a new object to reference later, stashed in the current layer
  out$layers[[current_layer]] <- insert_override(data = data,
                                                 layer_obj = out$layers[[current_layer]],
                                                 current_layer = current_layer,
                                                 mapping = mapping,
                                                 ignore_mapping = c("x", "y", "yend", "xend"))

  out$layers[[current_layer]]$swim_class <- "ggswim"

  # Return object
  out
}
