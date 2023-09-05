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
#' @importFrom ggplot2 aes geom_point geom_label

add_marker <- function(
    data = NULL,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  labels <- ifelse("label" %in% names(mapping), TRUE, FALSE)

  if (labels) {
    mapping$colour <- mapping$label # force color mapping with labels

    out <- geom_label(
      data = data,
      mapping = mapping,
      ...
    )

    out$swim_class <- "marker_label"
  } else {
    out <- geom_point(
      data = data,
      mapping = mapping,
      ...
    )

    out$swim_class <- "marker_point"
  }

  out
}


#' @title legend fixing
#'
#' @description
#' A short description...
#'
#' @param ggswim_obj description

fix_legend <- function(ggswim_obj) {

  # Determine indices of layers in ggplot object that contain labels and points
  label_layers <- c()
  point_layers <- c()
  override <- list(
    "colour" = list(
      "shape" = NULL,
      "color" = NULL,
      "label" = NULL
    )
  )

  for (i in seq_along(ggswim_obj$layers)) {
    if (ggswim_obj$layers[[i]]$swim_class == "marker_label") {
      label_layers <- c(label_layers, i)
    }

    if (ggswim_obj$layers[[i]]$swim_class == "marker_point") {
      point_layers <- c(point_layers, i)
    }
  }

  for(i in label_layers) {
    override[[i]] <- insert_override(
      data = ggswim_obj$layers[[i]]$data,
      current_layer = i,
      mapping = ggswim_obj$layers[[i]]$mapping,
      ignore_mapping = c("x", "y")
    )

    override$colour$colour = rep(NA, length(override$colour$colour))
  }

  for(i in point_layers) {
    override$colour <- insert_override(
      data = ggswim_obj$layers[[i]]$data,
      current_layer = i,
      mapping = ggswim_obj$layers[[i]]$mapping,
      ignore_mapping = c("x", "y")
    )
  }

  override
}
