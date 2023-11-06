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
#' More information about accepted mapping arguments can be found in **Aesthetics**.
#' @param ... Other arguments passed to `ggswim()`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#' @param arrow A column indicating what swim lanes should have arrows applied.
#' The column must be a logical data type (T/F).
#' @param arrow_colour Border/line color to use for the arrow. Default "black".
#' @param arrow_length A unit specifying the length of the arrow head (from tip to base).
#' Must be a ggplot2 `unit()` object. Default `ggplot2::unit(0.25, "inches")`.
#' @param arrow_fill Fill color/colour to use for the arrow head (if closed). Default `NULL`.
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head should
#' be a closed triangle. Default "closed."
#'
#' @section Aesthetics:
#' `ggswim()` understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `fill`
#' - `group`
#' - `linetype`
#' - `linewidth`
#'
#' **Note**: `ggswim()` **does not** support mapping using `color`/`colour`.
#'
#' @export
#'
#' @examples
#' ggswim(
#'   data = patient_status,
#'   mapping = aes(
#'     x = value,
#'     y = subject_id,
#'     fill = cohort
#'   )
#' )
ggswim <- function(
    data,
    mapping = aes(),
    arrow = NULL,
    arrow_colour = "black",
    arrow_length = unit(0.25, "inches"),
    arrow_fill = NULL,
    arrow_type = "closed",
    ...) {
  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = c("color", "colour"),
    parent_func = "add_marker()"
  )

  # TODO: Finalize, determine if this is acceptable to enforce
  # Attempt to extract original y variable and coerce to factor
  original_y_var <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "y")
  data[[original_y_var]] <- data[[original_y_var]] |> as.factor()

  # original_x_var <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "x")
  # data[[original_x_var]] <- data[[original_x_var]] |> as.factor()

  # Create ggplot and geom_col layers ----
  out <- data |>
    ggplot() +
    geom_col(
      mapping,
      ...
    )

  # Define new class 'ggswim_obj'
  class(out) <- c("ggswim_obj", class(out))
  current_layer <- length(out$layers) # The max length can be considered the current working layer

  # Add a reference class to the layer attributes
  attributes(out$layers[[current_layer]])$swim_class <- "ggswim"

  # Handle arrows ----
  arrow <- enquo(arrow) |> get_expr()
  if (!is.null(arrow)) {
    out <- add_arrows(
      data = data,
      ggswim_obj = out,
      mapping = mapping,
      arrow = arrow,
      arrow_colour = arrow_colour,
      arrow_type = arrow_type,
      arrow_fill = arrow_fill,
      arrow_length = arrow_length
    )
  }

  # Return object
  out
}

#' @title Add arrows to plot using geom_segment
#'
#' @description This helper function is triggered when a user requests arrows to
#' appear in `ggswim()`. It uses the `geom_segment()` function to supply them by
#' Adding a 0-length segment at the end of the swim lanes and then tacking on
#' arrows using the `arrow` argument.
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param ggswim_obj A ggswim object
#' @param arrow A column indicating what swim lanes should have arrows applied.
#' The column must be a logical data type (T/F).
#' @param arrow_colour Border/line color to use for the arrow. Default "black".
#' @param arrow_length A unit specifying the length of the arrow head (from tip to base).
#' Must be a ggplot2 `unit()` object. Default `ggplot2::unit(0.25, "inches")`.
#' @param arrow_fill Fill colour to use for the arrow head (if closed). Default `NULL`.
#' @param arrow_type One of "open" or "closed" indicating whether the arrow head should
#' be a closed triangle. Default "closed."
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' More information about accepted mapping arguments can be found in **Aesthetics**.
#' @param ... Other arguments passed to `ggswim()`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @keywords internal

add_arrows <- function(data,
                       ggswim_obj,
                       mapping,
                       arrow,
                       arrow_colour,
                       arrow_length,
                       arrow_fill,
                       arrow_type) {
  # Implement UI checks ----
  # Check that warning supplied if `arrow_fill` !NULL and `arrow_type` "open"
  check_arg_is_logical(data[[arrow]])
  check_arrow_fill_type(arrow_type, arrow_fill)

  x_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "x")
  y_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "y")

  xend <- NULL # define to avoid global variable note

  true_arrow_data <- data[data[arrow] == TRUE, ] |>
    mutate(
      .by = all_of(y_val),
      xend = sum(.data[[x_val]])
    )

  arrow_neck_length <- max(true_arrow_data$xend) * 0.15 # TODO: Determine better default

  out <- ggswim_obj +
    geom_segment(true_arrow_data,
      mapping = aes(
        x = xend,
        y = .data[[y_val]],
        yend = .data[[y_val]],
        xend = xend + arrow_neck_length
      ), colour = arrow_colour,
      arrow = arrow(
        type = arrow_type,
        length = arrow_length
      ),
      arrow.fill = arrow_fill
    )

  current_layer <- length(out$layers) # The max length can be considered the current working layer

  # Add a reference class to the layer attributes
  attributes(out$layers[[current_layer]])$swim_class <- "ggswim"

  out
}
