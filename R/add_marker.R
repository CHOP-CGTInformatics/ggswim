#' @title Add markers of interest to level response trajectories
#'
#' @description
#' "Markers" are used to specify events of interest along response trajectories
#' across individual lanes.
#'
#' @returns A ggswim object
#'
#' @param data a dataframe prepared for use with `ggswim()`, either coming from
#' a parent `ggswim()` function, another `add_marker()` call, or a new dataframe
#' prepared for use with `ggswim()`.
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param ... Other arguments passed to `geom_point`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @export
#'

add_marker <- function(
    data = NULL,
    mapping = aes(),
    ...
) {

  labels <- ifelse("label" %in% names(mapping), TRUE, FALSE)

  if (labels) {
    out <- geom_label(
      data = data,
      mapping = mapping,
      ...
    )

    # Tag the layer with a reference label
    out$swim_class <- "marker_label"

  } else {

    dots <- rlang::dots_list(...)
    static_colours <- NULL

    # Artificially create a column that will serve as the aes mapping "color" column
    # Currently dependent on "name" in mapping aes
    if ("name" %in% names(mapping)) {
      data <- data |>
        dplyr::mutate(!!mapping$name := mapping$name)

      mapping$colour <- rlang::sym(mapping$name)
      mapping <- mapping[names(mapping) != "name"] # remove name

      static_colours <- if ("color" %in% names(rlang::dots_list(...))){
        rlang::dots_list(...)$color
      } else {
        rlang::dots_list(...)$colour
      }

      dots <- rlang::dots_list(...)[!names(rlang::dots_list(...)) %in% c("color", "colour")]
    }

    out <- geom_point(
      data = data,
      mapping = mapping,
      colour = dots[names(dots) %in% c("color", "colour")],
      ...
    )

    if (is.null(dots$colour) & is.null(dots$color)) {
      out$aes_params$colour <- NULL
    }

    out$static_colours <- static_colours

    # Tag the layer with a reference label
    out$swim_class <- "marker_point"
  }

  out
}
