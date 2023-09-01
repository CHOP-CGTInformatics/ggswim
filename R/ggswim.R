#' @title Plot individual level response trajectories
#'
#' @description
#' Visualize individual record response trajectories over time using a swimmer plot.
#'
#' @details
#' A swimmer plot is a data visualization used to display individual
#' subject data over time. It shows events or outcomes as points along a
#' horizontal line for each subject, allowing easy comparison and pattern
#' identification.
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ... Other arguments passed to `geom_col`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @export
#'
#' @importFrom ggplot2 aes ggplot geom_col

ggswim <- function(
    data,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  # TODO: Finalize, determine if this is acceptable to enforce
  data[[mapping$y |> get_expr()]] <- data[[mapping$y |> get_expr()]] |> as.factor()

  out <- data |>
    ggplot() +
    geom_col(
      mapping,
      ...
    )

  # Define new class 'ggswim_obj'
  class(out) <- c("ggswim_obj", class(out))
  current_layer <- length(out$layers) # The max length can be considered the current working layer

  # TODO: Determine if necessary to keep overrides
  # Define a new object to reference later, stashed in the current layer
  out$layers[[current_layer]] <- insert_override(data = data,
                                                 layer_obj = out$layers[[current_layer]],
                                                 current_layer = current_layer,
                                                 mapping = mapping,
                                                 ignore_mapping = c("x", "y"))

  # TODO: Determine if necessary to keep layer reference value
  # Add a reference class to the layer
  out$layers[[current_layer]]$swim_class <- "ggswim"

  # Return object
  out
}
