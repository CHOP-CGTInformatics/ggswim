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
#'   )
#' )
#'
#' # ggswim call with arrows
#' ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = start_time,
#'     xend = end_time,
#'     y = pt_id,
#'     color = disease_assessment
#'   ),
#'   arrow = status,
#'   arrow_fill = "forestgreen",
#'   arrow_colour = "cyan",
#'   arrow_head_length = ggplot2::unit(.25, "inches"),
#'   arrow_neck_length = status_length
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

  # Define new class 'ggswim_obj'
  class(out) <- c("ggswim_obj", class(out))
  # The max length can be considered the current working layer
  # TODO: Determine if holds true, or should hold true, when adding ggswim layer
  # onto an existing ggplot
  current_layer <- length(out$layers)

  # Add a reference class to the layer attributes
  attributes(out$layers[[current_layer]])$swim_class <- "ggswim"

  # Detect arrows ----
  arrow <- enquo(arrow) |> get_expr()
  arrow_neck_length <- if (quo_is_symbolic(quo(arrow_neck_length))) {
    enquo(arrow_neck_length) |> get_expr()
  } else {
    arrow_neck_length
  }
  has_arrows <- !is.null(arrow)

  if (has_arrows) {
    out <- out +
      add_arrows(
        data = data,
        mapping = mapping,
        position = position,
        arrow = {{arrow}},
        arrow_colour = arrow_colour,
        arrow_type = arrow_type,
        arrow_fill = arrow_fill,
        arrow_head_length = arrow_head_length,
        arrow_neck_length = {{arrow_neck_length}}
    )
  }

  # Return object
  out + new_scale_color()
}

#' @title Add arrows to a swimmer plot
#'
#' @description
#' Add arrows to the ends of swimmer plot lanes to indicate unknown statuses
#' or continued record-level trajectories.
#'
#' @details
#' `add_arrows()` wraps a new [geom_segment()] layer by adding a zero-length
#' segment at the right end of swimmer lanes. This approach allows users to
#' specify `arrow_neck_length` which can be useful for tracking and visualizaing
#' time in between markers
#'
#' @param data a dataframe prepared for use with [ggswim()]
#' @inheritParams ggswim
#' @param ... Other arguments passed to `ggswim()`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @examples
#' add_arrows(data = patient_status,
#'            mapping = aes(xend = end_time, y = pt_id),
#'            arrow = arrow,
#'            arrow_neck_length = time_from_today,
#'            arrow_colour = "forestgreen",
#'            arrow_fill = "forestgreen")
#'
#' @export

add_arrows <- function(data = NULL,
                       mapping = NULL,
                       position = "identity",
                       arrow = NULL,
                       arrow_colour = "black",
                       arrow_head_length = unit(0.25, "inches"),
                       arrow_neck_length = NULL,
                       arrow_fill = NULL,
                       arrow_type = "closed") {
  # Handle dynamic arrow vars ----
  arrow <- enquo(arrow) |> get_expr()
  arrow_neck_length <- if (quo_is_symbolic(quo(arrow_neck_length))) {
    enquo(arrow_neck_length) |> get_expr()
  } else {
    arrow_neck_length
  }

  # Implement UI checks ----
  # Check that warning supplied if `arrow_fill` !NULL and `arrow_type` "open"
  check_arg_is_logical(data[[arrow]])
  check_arrow_fill_type(arrow_type, arrow_fill)
  check_arrow_neck_length(arrow_neck_length)

  x_val <- retrieve_original_aes(data, aes_mapping = unlist(mapping), aes_var = "xend") # nolint: object_usage_linter
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
  out <- geom_segment(true_arrow_data,
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

  # Add a reference class to the layer attributes
  attributes(out)$swim_class <- "ggswim_arrows"

  out
}
