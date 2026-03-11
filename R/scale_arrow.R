#' @title Position scales for discrete arrow data
#'
#' @description
#' [scale_arrow_discrete()] is used to set discrete arrow aesthetics for swimmer
#' plot arrows.
#'
#' @param colours Arrow outline colours passed to the arrow layer.
#' @param fills Arrow fill colours passed to the arrow layer for closed arrows.
#' @param types Arrow head types passed to the arrow layer. One of `"open"` or
#' `"closed"`.
#' @param limits Arrow labels passed to the arrow scale.
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritParams ggplot2::discrete_scale
#'
#' @examples
#' \dontrun{
#' ggplot2::ggplot() +
#'   geom_swim_arrow(
#'     data = arrow_data,
#'     aes(xend = end_time, y = pt_id, arrow = label)
#'   ) +
#'   scale_arrow_discrete(
#'     colours = "black",
#'     fills = "black",
#'     types = "closed",
#'     limits = "Continuation"
#'   )
#' }
#'
#' @export
scale_arrow_discrete <- function(colours = NULL, fills = NULL, types = NULL, limits = NULL, ...) {
  n_values <- max(c(length(colours), length(fills), length(types), length(limits)))

  if (n_values == 0) {
    arrows <- data.frame()
  } else {
    arrows <- data.frame(
      colours = colours %||% rep(.default_arrow_colours, length.out = n_values),
      fills   = fills %||% rep(.default_arrow_fills, length.out = n_values),
      types   = types %||% rep(.default_arrow_types, length.out = n_values),
      labels  = limits %||% rep(.default_arrow_limits, length.out = n_values)
    ) |>
      dplyr::distinct()
  }

  palette <- pal_arrows(
    colours = arrows$colours,
    fills = arrows$fills,
    types = arrows$types,
    n_values = nrow(arrows)
  )

  discrete_scale(
    aesthetics = "arrow",
    palette = palette,
    limits = arrows$labels,
    ...,
    na.translate = FALSE
  )
}

#' @noRd
#' @keywords internal
pal_arrows <- function(colours = NULL, fills = NULL, types = NULL, n_values = NULL) {
  # Define colour, fill, and types lengths via arrow args supplied or default values
  n_values <- n_values %||% max(length(colours), length(fills), length(types))
  if (n_values == 0) n_values <- length(.default_arrow_limits)
  # Create a vctrs list to store colour, fill, and types values
  arrows <- vctrs::new_rcrd(
    list(
      colour = rep(colours %||% .default_arrow_colours, length.out = n_values),
      fill   = rep(fills %||% .default_arrow_fills, length.out = n_values),
      type   = rep(types %||% .default_arrow_types, length.out = n_values)
    ),
    class = "swim_arrow"
  )

  # If the values supplied are of a length greater than the default palette,
  # throw a warning.
  function(n) {
    if (n > n_values) {
      cli::cli_warn(
        "This palette can handle a maximum of {n_values} values. \\
        You have supplied {n}."
      )
    }
    arrows[seq_len(n)]
  }
}

#' @title ggswim arrow defaults
#'
#' @examples
#' ggswim::.default_arrow_colours
#' ggswim::.default_arrow_fills
#' ggswim::.default_arrow_types
#' ggswim::.default_arrow_limits
#'
#' @export
.default_arrow_colours <- "black"

#' @rdname dot-default_arrow_colours
#' @export
.default_arrow_fills <- "black"

#' @rdname dot-default_arrow_colours
#' @export
.default_arrow_types <- "closed"

#' @rdname dot-default_arrow_colours
#' @export
.default_arrow_limits <- "Continuation"

#' @export
#' # Define the format method for swim_arrow class
format.swim_arrow <- function(x, ...) {
  colours <- vctrs::field(x, "colour")
  fills <- vctrs::field(x, "fill")
  types <- vctrs::field(x, "type")

  # Create a formatted string for each arrow
  # Return the formatted arrows as a character vector
  paste0("Arrow: colour=", colours, ", fill=", fills, ", type=", types)
}
