#' @title Create swimmer survival plots
#'
#' @description
#' Use ggplot2 architecture to create a swimmer plot showing subject survival
#' timelines.
#'
#' @details
#' A swimmer plot is a data visualization used to display individual
#' subject data over time. It shows events or outcomes as points along a
#' horizontal line for each subject, allowing easy comparison and pattern
#' identification.
#'
#' @param data a dataframe prepared for use with [ggswim()]
#' @inheritParams ggplot2::geom_segment
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#'
#' @section Aesthetics:
#' `ggswim()` understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - **xend _or_ yend**
#' - `alpha`
#' - `colour`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' `ggswim()` is a wrapper for [geom_segment()] and can support much of the same
#' functionality.
#'
#' **Notes**:
#'
#' - `ggswim()` **does not** support mapping using `fill`.
#'
#' @section Arrows:
#' Arrows can be specified in `ggswim()` as well as via the separate function,
#' [add_arrows()].
#'
#' @export
#'
#' @examples
#' # Simple ggswim call
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = start_time,
#'     xend = end_time,
#'     y = pt_id,
#'     color = disease_assessment
#'   ),
#'   linewidth = 5
#' )

ggswim <- function(
    data,
    mapping = aes(),
    position = "identity",
    ...) {
  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "ggswim()"
  )

  check_missing_params(
    mapping = mapping,
    params = c("x", "xend", "y"),
    parent_func = "ggswim()"
  )

  check_supported_position_args(
    position = position,
    parent_func = "ggswim()"
  )

  # TODO: Finalize, determine if this is acceptable to enforce
  # Attempt to extract original y variable and coerce to factor
  original_y_var <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "y")
  data[[original_y_var]] <- data[[original_y_var]] |> as.factor()

  # Create ggplot and geom_segment layers ----
  out <- data |>
    ggplot() +
    geom_segment(
      mapping,
      position = position,
      ...
    )

  # Define new class 'ggswim_obj' (after new color scale)
  class(out) <- c("ggswim_obj", class(out))
  # The max length can be considered the current working layer
  # TODO: Determine if necessary, ggswim currently does not work with an existing
  # ggplot. We may want to make this available in the future.
  current_layer <- length(out$layers)

  # Add a reference class to the layer attributes
  out$layers[[current_layer]]$swim_class <- "ggswim"

  # Return
  out
}
