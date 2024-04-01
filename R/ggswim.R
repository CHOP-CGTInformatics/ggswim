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
#' @inheritParams ggplot2::geom_col
#' @param position Position adjustment. ggswim accepts either "stack", or "identity"
#' depending on the use case. Default "identity".
#' @param arrow A column indicating what swim lanes should have arrows applied.
#' The column must be a logical data type (T/F).
#' @param arrow_colour Border/line color to use for the arrow. Default "black".
#' @param arrow_fill Fill color/colour to use for the arrow head (if closed). Default `NULL`.
#' @param arrow_head_length A unit specifying the length of the arrow head (from tip to base).
#' Must be a ggplot2 `unit()` object. Default `ggplot2::unit(0.25, "inches")`.
#' @param arrow_neck_length The length of the neck of the arrow from the end of a
#' swim lane to the base of the arrow head. Either an integer value or a column
#' specifier. The default, `NULL`, sets a value proportional to the max lane value.
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
#' # Simple ggswim call
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = delta_t0_months,
#'     y = pt_id,
#'     fill = disease_assessment_status
#'   )
#' )
#'
#' # ggswim call with arrows
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = delta_t0_months,
#'     y = pt_id,
#'     fill = disease_assessment_status
#'   ),
#'   arrow = arrow_status,
#'   arrow_fill = "cyan",
#'   arrow_head_length = unit(.25, "inches"),
#'   arrow_neck_length = delta_today
#' )

ggswim <- function(
    data,
    mapping = aes(),
    position = "identity",
    arrow = NULL,
    arrow_colour = "black",
    arrow_head_length = unit(0.25, "inches"),
    arrow_neck_length = NULL,
    arrow_fill = NULL,
    arrow_type = "closed",
    ...) {
  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = c("color", "colour"),
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

  # Create ggplot and geom_col layers ----
  out <- data |>
    ggplot() +
    geom_col(
      mapping,
      position = position,
      ...
    )

  # Define new class 'ggswim_obj'
  class(out) <- c("ggswim_obj", class(out))
  # The max length can be considered the current working layer
  # TODO: Determine if holds true, or should hold true, when adding ggswim layer
  # onto an existing ggplot
  current_layer <- length(out$layers)

  # Add a reference class to the layer attributes
  attributes(out$layers[[current_layer]])$swim_class <- "ggswim"

  # Handle arrows ----
  arrow <- enquo(arrow) |> get_expr()
  arrow_neck_length <- if (quo_is_symbolic(quo(arrow_neck_length))) {
    enquo(arrow_neck_length) |> get_expr()
  } else {
    arrow_neck_length
  }

  if (!is.null(arrow)) {
    out <- add_arrows(
      data = data,
      ggswim_obj = out,
      mapping = mapping,
      position = position,
      arrow = arrow,
      arrow_colour = arrow_colour,
      arrow_type = arrow_type,
      arrow_fill = arrow_fill,
      arrow_head_length = arrow_head_length,
      arrow_neck_length = arrow_neck_length
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
#' @inheritParams ggswim
#' @param ... Other arguments passed to `ggswim()`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @keywords internal

add_arrows <- function(data,
                       ggswim_obj,
                       mapping,
                       position,
                       arrow,
                       arrow_colour,
                       arrow_head_length,
                       arrow_neck_length,
                       arrow_fill,
                       arrow_type) {
  # Implement UI checks ----
  # Check that warning supplied if `arrow_fill` !NULL and `arrow_type` "open"
  check_arg_is_logical(data[[arrow]])
  check_arrow_fill_type(arrow_type, arrow_fill)
  check_arrow_neck_length(arrow_neck_length)

  x_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "x") # nolint: object_usage_linter
  y_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "y")

  xend <- NULL # define to avoid global variable note

  # Filter for only data where arrows are TRUE, sum xend for instances of non-zero
  # start values
  true_arrow_data <- data[data[arrow] == TRUE, ] |>
    mutate(
      .by = all_of(y_val),
      xend = case_when(
        position == "identity" ~ max(.data[[x_val]], na.rm = TRUE),
        position == "stack" ~ sum(.data[[x_val]], na.rm = TRUE),
        TRUE ~ NA
      )
    )

  # If NULL, neck length to be a 0.15 proportion
  if (is.null(arrow_neck_length)) {
    arrow_neck_length <- max(true_arrow_data$xend) * 0.15
  }

  out <- ggswim_obj +
    geom_segment(true_arrow_data,
      mapping = aes(
        x = xend,
        y = .data[[y_val]],
        yend = .data[[y_val]],
        xend = if (is.name(arrow_neck_length)) {
          xend + .data[[arrow_neck_length]]
        } else {
          xend + arrow_neck_length
        },
      ), colour = arrow_colour,
      arrow = arrow(
        type = arrow_type,
        length = arrow_head_length
      ),
      arrow.fill = arrow_fill
    )

  current_layer <- length(out$layers) # The max length can be considered the current working layer

  # Add a reference class to the layer attributes
  attributes(out$layers[[current_layer]])$swim_class <- "ggswim"

  out
}
