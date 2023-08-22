#' @title ggswim title
#'
#' @description
#' A short description...
#'
#' @param data a dataframe prepared for use with `ggswim()`
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time the x-axis variable of the swimmer plot, typically a
#' function of time given in date or numeric format
#' @param lanes a column in `df` that represents segments of the swimmer plot
#'
#' @export
#'
#' @importFrom ggplot2 aes ggplot geom_segment
#' @importFrom rlang enquo get_expr
#' @importFrom dplyr arrange mutate row_number lag

ggswim <- function(
    data,
    id,
    time,
    lane
) {
  # Capture variables as expressions, allowing for piping in API
  variables <- c("id", "time", "lane")

  # Parse variables to be passed to streamline()
  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  # Pre-processing
  data_new <- data |>
    arrange(.by = !!id, !!id, max(!!time), !!time) |>
    mutate(
      .by = !!id,
      xstart = ifelse(row_number() == 1, 0, lag(!!time)),
      .before = !!time
    )

  data_new[[id]] <- factor(data_new[[id]], levels = rev(unique(data_new[[id]])))

  out <- data_new |>
    ggplot() +
    geom_segment(
      aes(x = xstart, y = !!id, xend = !!time, yend = !!id, colour = !!lane),
      linewidth = 10
    )

  # Define a new object to reference later, stashed in the ggplot object
  out$guide_overrides <- list()
  out$guide_capture <- list()

  # Return object
  out
}
