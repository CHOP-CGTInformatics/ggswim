#' @title add_marker title
#'
#' @description
#' A short description...
#'
#' @param ggswim_obj a ggswim_obj
#' @param data data where markers reside
#' @param id the id variable associated with the marker
#' @param time the timestamp location for where to place the markers
#' @param ... Other arguments passed on to layer(). These are often aesthetics,
#' used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @export
#'
#' @importFrom ggplot2 aes ggplot geom_point
#' @importFrom rlang enquo get_expr
#' @importFrom dplyr arrange mutate row_number

add_marker <- function(
    ggswim_obj,
    data,
    id,
    time,
    ...
    ) {
  # Capture variables as expressions, allowing for piping in API
  variables <- c("id", "time")

  # Parse variables to be passed to streamline()
  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  ggswim_obj +
    geom_point(data = data, aes(x = !!time, y = !!id), ...)

}
