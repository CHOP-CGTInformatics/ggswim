#' @title Streamline the ggswim legend
#'
#' @description
#' A short description...
#'
#' @param ...
#'
#' @returns a ggswim object
#'
#' @export

streamline_legend <- function(
    ...,
    environment = parent.frame()
) {
  # Capture ggswim plot object for manipulating
  ggswim_obj <- get_ggswim_obj_from_env(environment)

  #TODO: Support multiple of each kind of layer
  ggswim_layer <- filter_list_elements(ggswim_obj$layers, "swim_class", "ggswim")
  marker_layer <- filter_list_elements(ggswim_obj$layers, "swim_class", "marker")

  ggswim_obj +
   guides(
     #TODO: support and remove colors that have relocated to fill for geom_point
     # color = guide_legend(
     #   override.aes = list(
     #     shape = ggswim_layer[[1]]$overrides$shape,
     #     size = ggswim_layer[[1]]$overrides$size,
     #     colour = ggswim_layer[[1]]$overrides$colour, #TODO: Support color and colour
     #     stroke = ggswim_layer[[1]]$overrides$stroke
     #   )
     # ),
     fill = guide_legend(
       override.aes = list(
         shape = marker_layer[[1]]$overrides$shape,
         size = marker_layer[[1]]$overrides$size,
         colour = marker_layer[[1]]$overrides$colour, #TODO: Support color and colour
         stroke = marker_layer[[1]]$overrides$stroke
       )
     )
   )
}

#' @title Find matching list element
#'
#' @description
#' A short description...
#'
#' @returns a list
#' @keywords internal

filter_list_elements <- function(input_list, name_to_match, value_to_match) {
  matched_elements <- list()

  for (element in input_list) {
    if (identical(element[[name_to_match]], value_to_match)) {
      matched_elements <- append(matched_elements, list(element))
    }
  }

  return(matched_elements)
}


