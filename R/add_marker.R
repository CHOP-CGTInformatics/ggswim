#' @title add_marker title
#'
#' @description
#' A short description...
#'
#' @param ggswim_obj a ggswim_obj
#' @param data data where markers reside, if in ggswim object leave `NULL` (default)
#' @param id the id variable associated with the marker
#' @param time the timestamp location for where to place the markers
#' @param name the name of the marker. Default `NULL`
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
    data = NULL,
    id,
    time,
    name,
    ...
    ) {
  # Capture variables as expressions, allowing for piping in API
  variables <- c("id", "time", "name")

  # Parse variables to be passed to streamline()
  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  # If no new dataset applied, use data from the original ggswim_obj
  if (is.null(data)) {
    data <- ggswim_obj$data
  }

  # Add a fill reference point
  data_new <- data |>
    mutate(
      fill = name
    )

  # TODO: Make this ubiquitous? Currently copied in ggswim_new function
  data_new[[id]] <- factor(data_new[[id]], levels = rev(unique(data_new[[id]])))

  out <- ggswim_obj +
    geom_point(data = data_new, aes(x = !!time, y = !!id, fill = fill), ...)

  # Since we just made a new layer, the max layer (the length) is therefor the "current" one
  current_layer <- length(out$layers)
  # Insert a name based on the name provided into the layer params to help with legend guide defs
  out$layers[[current_layer]]$aes_params$name <- name

  appender_list_element <- list(temp = out$layers[[current_layer]]$aes_params)
  names(appender_list_element) <- name #TODO: potentially use rlang to make this more elegant

  out$guide_capture <- append(out$guide_capture, values = appender_list_element)
  #TODO: Make prettier
  out$guide_overrides$colour_override <- append(out$guide_overrides$colour_override, out$guide_capture[[name]]$colour)
  names(out$guide_overrides$colour_override)[length(out$guide_overrides$colour_override)] <- name
  out$guide_overrides$stroke_override <- append(out$guide_overrides$stroke_override, out$guide_capture[[name]]$stroke)
  names(out$guide_overrides$stroke_override)[length(out$guide_overrides$stroke_override)] <- name
  out$guide_overrides$shape_override <- append(out$guide_overrides$shape_override, out$guide_capture[[name]]$shape)
  names(out$guide_overrides$shape_override)[length(out$guide_overrides$shape_override)] <- name
  out$guide_overrides$size_override <- append(out$guide_overrides$size_override, out$guide_capture[[name]]$size)
  names(out$guide_overrides$size_override)[length(out$guide_overrides$size_override)] <- name

  out +
    guides(
      fill = guide_legend(
        override.aes = list(
          shape = out$guide_overrides$shape_override,
          colour = out$guide_overrides$colour_override,
          stroke = out$guide_overrides$stroke_override
        )
      )
    )
}
