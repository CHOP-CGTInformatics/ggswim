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
#' @importFrom dplyr arrange mutate row_number

ggswim_new <- function(
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
    arrange(.by = id, id, max(time), time) |>
    mutate(
      .by = id,
      xstart = ifelse(row_number() == 1, 0, lag(time)),
      .before = time
    ) |>
    mutate(id = factor(id, levels = rev(unique(id))))

  data_new |>
    ggplot() +
    geom_segment(
      aes(x = xstart, y = id, xend = time, yend = id, colour = !!lane),
      linewidth = 10
    )
}
