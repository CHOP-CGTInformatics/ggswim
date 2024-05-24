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
#' print(
#'   ggplot2::ggplot(data = patient_data) +
#'     geom_swim_lane(
#'       mapping = aes(
#'         x = start_time,
#'         y = pt_id,
#'         xend = end_time,
#'         color = disease_assessment
#'       )
#'     )
#' )
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
#' print(
#'   geom_swim_lane(
#'     data = patient_data,
#'     mapping = aes(
#'       x = start_time,
#'       y = pt_id,
#'       xend = end_time,
#'       color = disease_assessment
#'     )
#'   )
#' )
NULL

#' @export
#' @rdname print.ggswim_layer
print.ggswim_layer <- function(x, ..., flat = TRUE) {
  # Print mapping
  mapping <- attr(x, "mapping")
  if (!is.null(mapping)) {
    mapping_strings <- tibble(name = names(mapping), value = as.character(mapping)) |>
      mutate(mapping_string = paste(.data$name, "=", .data$value, sep = " ")) |>
      pull(.data$mapping_string)
    cat("mapping", paste(mapping_strings, collapse = ", "), "\n")
  }

  # Print name
  cat(paste0(x, ": "))

  # Print params
  # Extract and process params using dplyr
  params <- attr(x, "params")
  param_strings <- tibble(name = names(params), value = params) |>
    mutate(value = if_else(map_lgl(.data$value, is.null), "NULL", as.character(.data$value))) |>
    mutate(param_string = paste(.data$name, "=", .data$value)) |>
    pull(.data$param_string)

  cat(paste(param_strings, collapse = ", "), "\n")

  # Print non params
  attrs <- attributes(x)
  other_attrs <- names(attrs) |>
    setdiff(c("params", "class", "names"))

  attrs |>
    enframe() |>
    filter(.data$name %in% other_attrs) |>
    mutate(output = case_when(
      name == "stat" ~ paste0(
        name, "_", value, ": na.rm = ",
        if_else(is.null(params$na.rm), "NULL", as.character(params$na.rm))
      ),
      name == "position" ~ paste0(name, "_", value),
      TRUE ~ NA_character_
    )) |>
    pull(.data$output) |>
    discard(is.na) |>
    walk(cat, "\n")
}
