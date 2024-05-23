#' @title Print ggswim object
#'
#' @param x a ggswim object
#' @param ... not used
#'
#' @name print.ggswim_obj
#' @return a ggplot
#' @keywords internal
#'
#' @examples
#' ggplot2::ggplot(data = patient_data) +
#'   geom_swim_lane(
#'     mapping = aes(
#'       x = start_time,
#'       y = pt_id,
#'       xend = end_time,
#'       color = disease_assessment
#'     )
#'   ) |>
#'   print()
NULL

#' @export
#' @rdname print.ggswim_obj
print.ggswim_obj <- function(x, ...) {
  build_ggswim(x) |> print()
}

#' @title Print ggswim layer object
#'
#' @param x a ggswim layer object
#' @param ... not used
#'
#' @name print.ggswim_layer
#' @return a layer object
#' @keywords internal
#'
#' @examples
#' geom_swim_lane(
#'   data = patient_data,
#'   mapping = aes(
#'     x = start_time,
#'     y = pt_id,
#'     xend = end_time,
#'     color = disease_assessment
#'   )
#' ) |>
#'   print()
NULL

#' @export
#' @rdname print.ggswim_layer
print.ggswim_layer <- function(x, ..., flat = TRUE) {
  cat(paste0(x, ": "))

  # Print params attributes
  params <- attr(x, "params")
  param_strings <- sapply(names(params), function(param) {
    value <- params[[param]]
    if (is.null(value)) {
      value <- "NULL"
    }
    paste0(param, " = ", value)
  })
  cat(paste(param_strings, collapse = ", "), "\n")

  # Print other attributes
  attrs <- attributes(x)
  other_attrs <- setdiff(names(attrs), c("params", "class", "names"))
  for (attr_name in other_attrs) {
    if (attr_name == "stat" || attr_name == "position") {
      if (attr_name == "stat") {
        cat(paste0(attr_name, "_", attrs[[attr_name]], ": na.rm = ", params$na.rm, "\n"))
      } else {
        cat(paste0(attr_name, "_", attrs[[attr_name]], "\n"))
      }
    }
  }
}
