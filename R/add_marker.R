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
    out <- geom_point(
      data = data,
      mapping = mapping,
      ...
    )

    # Tag the layer with a reference label
    out$swim_class <- "marker_point"
  }

  out
}
