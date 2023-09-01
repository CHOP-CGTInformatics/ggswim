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
#' @importFrom ggplot2 aes geom_point

add_marker <- function(
    data = NULL,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  out <- geom_point(
    data = data,
    mapping = mapping,
    ...
  )

  # Capture ggswim plot object for manipulating - Only necessary for identifying
  # current layer and assigning overrides
  ggswim_obj <- get_ggswim_obj_from_env(environment)

  ref_plot <- ggswim_obj + out
  current_layer <- length(ref_plot$layers) # The max length can be considered the current working layer

  # TODO: Determine if necessary to keep overrides
  # Handling for override data when no new data given, i.e. data from further
  # up layer stack
  override_data <- if (is.null(data)) {ggswim_obj$data} else {data}

  out <- insert_override(data = override_data,
                         layer_obj = out,
                         current_layer = current_layer,
                         mapping = mapping,
                         ignore_mapping = c("x", "y"))

  # TODO: Determine if necessary to keep layer reference value
  # Add a reference class to the layer
  out$swim_class <- "marker"

  out
}

#' @title Get ggswim object from an enviornment variable
#'
#' @description
#' Determine the parent environment component containing a `ggswim_obj` class
#' type and return the object
#'
#' @keywords internal
#'
#' @returns A ggswim object
#'
#' @param env an environment object

get_ggswim_obj_from_env <- function(env) {
  sub_objects <- ls(env)

  ggswim_object <- list()

  # Loop through sub-objects and classify them
  for (sub_object in sub_objects) {
    obj <- get(sub_object, envir = env)
    obj_class <- class(obj)
    if ("ggswim_obj" %in% obj_class) {
      ggswim_object <- obj
    }
  }

  ggswim_object
}
