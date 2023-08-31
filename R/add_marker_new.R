#' @title add_marker title
#'
#' @description
#' A short description...
#'
#' @returns A ggswim object
#'
#' @param data data where markers reside, if in ggswim object leave `NULL` (default)
#' @param ... Other arguments passed on to layer(). These are often aesthetics,
#' used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @export
#'
#' @importFrom ggplot2 aes geom_point

add_marker_new <- function(
    data = NULL,
    mapping = aes(),
    ...,
    environment = parent.frame()
) {

  # Capture ggswim plot object for manipulating
  ggswim_obj <- get_ggswim_obj_from_env(environment)

  out <- geom_point(
      data = data,
      mapping = mapping,
      ...
    )

  #TODO: Might be non-standard, but for now necessary to get elements of the current layer requiring rendering
  ref_plot <- ggswim_obj + out
  current_layer <- length(ref_plot$layers) # The max length can be considered the current working layer

  out <- insert_override(data = data, #TODO: Handle data if NULL
                         layer_obj = out,
                         current_layer = current_layer,
                         mapping = mapping,
                         ignore_mapping = c("x", "y"))

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
#' @param env an enviornment object

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

    ggswim_object
  }

  ggswim_object
}
