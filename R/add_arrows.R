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
#'
#' @examples
#' \dontrun{
#' add_arrows(
#'   data = patient_status,
#'   mapping = aes(xend = end_time, y = pt_id),
#'   arrow = arrow,
#'   arrow_neck_length = time_from_today,
#'   arrow_colour = "forestgreen",
#'   arrow_fill = "forestgreen"
#' )
#'}
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

  # Add ggswim_obj class if none exists due to separate call
  # Define new class 'ggswim_obj' (after new color scale)
  class(out) <- c("ggswim_obj", class(out))

  # Add a reference class to the layer attributes
  out$swim_class <- "ggswim_arrows"

  out
}
