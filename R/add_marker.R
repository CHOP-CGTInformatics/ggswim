#' @title add_marker title
#'
#' @description
#' A short description...
#'
#' @returns A ggswim object
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
#' @importFrom ggplot2 aes ggplot geom_point guides guide_legend
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

  out <- apply_guide_overrides(out, name = name)

  # NA Handling, check if any NA values exist in the guide overrides
  if (any(sapply(out$guide_overrides, function(vector) any(is.na(vector))))) {
    out <- update_missing_guide_overrides(out)
  }

  out +
    guides(
      fill = guide_legend(
        override.aes = list(
          shape = out$guide_overrides$shape_override,
          size = out$guide_overrides$size_override,
          colour = out$guide_overrides$colour_override,
          stroke = out$guide_overrides$stroke_override
        )
      )
    )
}

#' @title Assign guide overrides
#'
#' @description
#' Helper function for assigning guide override values to a ggplot function for
#' use with `add_markers`
#'
#' @param ggswim_obj description
#' @param ... Additional arguments from the parent function
#'
#' @returns a ggswim object
#'
#' @importFrom stringr str_replace
#'
#' @keywords internal

apply_guide_overrides <- function(ggswim_obj, ...) {
  name <- list(...)$name # Name of current point layer, defined by arg "name"
  out <- ggswim_obj
  guide_overrides <- out$guide_overrides # For simplicity
  current_layer <- length(out$layers) # The max layer (i.e. the length) is "current"

  # Add all applicable aesthetic guides to a sub-list of out$guide_capture
  out <- append_guide_capture(out, current_layer, name)

  # Define element vectors for current and previous layers
  current_elements <- names(out$guide_capture[[name]])
  former_elements <- str_replace(names(out$guide_overrides), pattern = "_override", replacement = "")

  # Determine missing elements between former layer and current layer
  former_elements_missing_from_current_layer <- setdiff(former_elements, current_elements)
  # Determine missing elements between current layer and former layer
  current_elements_missing_from_former_layer <- setdiff(current_elements, former_elements)

  # If no elements added to guide_overrides, define default NA values for supported aesthetics
  first_marker_layer <- FALSE # Is this the first marker layer?
  if (length(guide_overrides) == 0) {
    first_marker_layer <- TRUE
    guide_overrides$colour_override <- NA
    guide_overrides$shape_override <- NA
    guide_overrides$stroke_override <- NA
    guide_overrides$size_override <- NA
  }

  # If layer elements specified and none exist in a former layer: ----
  if (length(current_elements) != 0 & length(former_elements_missing_from_current_layer) == 0){

    # Apply current elements
    for(element in current_elements) {
      override_name <- paste0(element, "_override")
      # Differentiate whether this is the first layer or the first element of a new layer
      guide_overrides[[override_name]] <- if (first_marker_layer) {
        out$guide_capture[[name]][[element]]
      } else {
        append(guide_overrides[[override_name]], out$guide_capture[[name]][[element]])
      }
    }
  }

  # If the current layer is missing elements from a previous layer: ----
  if (length(former_elements_missing_from_current_layer) != 0) {

    # Append current elements
    for(element in current_elements) {
      override_name <- paste0(element, "_override")
      guide_overrides[[override_name]] <- append(guide_overrides[[override_name]], out$guide_capture[[name]][[element]])
    }

    # Append NA for former layer elements not supplied in current layer
    for(element in former_elements_missing_from_current_layer) {
      override_name <- paste0(element, "_override")
      guide_overrides[[override_name]] <- append(guide_overrides[[override_name]], NA)
    }
  }

  # If the current layer contains new elements not in former layer: ----
  if (any(!current_elements_missing_from_former_layer %in% former_elements_missing_from_current_layer)) {

    # If former layer contains elements missing from current layer:
    if (length(former_elements_missing_from_current_layer) != 0) {

      # Append NAs equal to max length of current layer followed by override elements
      for (element in current_elements_missing_from_former_layer) {
        max_override_length <- max(sapply(guide_overrides, length))
        override_name <- paste0(element, "_override")
        guide_overrides[[override_name]] <- c(rep(NA, max_override_length - 1), guide_overrides[[override_name]])
      }
    }

  }

  out$guide_overrides <- guide_overrides
  out
}

#' @title Replace NA guide values with defaults
#'
#' @description
#' Switch NA values in `guide_override`s with default values
#'
#' @param ggswim_obj description
#' @param ... Additional arguments from the parent function
#'
#' @returns a ggswim object
#'
#' @keywords internal
#'
#' @importFrom scales hue_pal

update_missing_guide_overrides <- function(ggswim_obj, ...) {
  out <- ggswim_obj
  guide_overrides <- out$guide_overrides

  # Colour Override Handling ----
  if ("colour_override" %in% names(guide_overrides)) {
    # Number of default colors to generate
    num_defaults <- sum(is.na(guide_overrides$colour_override))

    # Generate default colors using scales::hue_pal()
    # default_colors <- hue_pal()(num_defaults)
    default_colors <- "black"

    # Replace NA values with default colors
    guide_overrides$colour_override[is.na(guide_overrides$colour_override)] <- default_colors
  }

  # Size Override Handling ----
  if ("size_override" %in% names(guide_overrides)) {
    # Number of default sizes to generate
    num_defaults <- sum(is.na(guide_overrides$size_override))

    # Replace NA values with default size, 1
    guide_overrides$size_override[is.na(guide_overrides$size_override)] <- 1
  }

  # Stroke Override Handling ----
  if ("stroke_override" %in% names(guide_overrides)) {
    # Number of default strokes to generate
    num_defaults <- sum(is.na(guide_overrides$stroke_override))

    # Replace NA values with default stroke, 1
    guide_overrides$stroke_override[is.na(guide_overrides$stroke_override)] <- 1
  }

  # Shape Override Handling ----
  if ("shape_override" %in% names(guide_overrides)) {
    # Number of default shapes to generate
    num_defaults <- sum(is.na(guide_overrides$shape_override))

    # Replace NA values with default stroke, 1
    guide_overrides$shape_override[is.na(guide_overrides$shape_override)] <- 19
  }

  out$guide_overrides <- guide_overrides
  out
}

#' @title Append guide_capture list elements
#'
#' @description
#' Internal utility function that appends sub lists onto the `guide_capture` list
#' of a `ggswim` object
#'
#' @keywords internal
#'
#' @param ggswim_obj ggswim_obj
#' @param current_layer the current ggplot layer
#' @param name name of the current layer, defined by the "name" argyument in `add_marker()`
#'
#' @returns a ggswim object

append_guide_capture <- function(ggswim_obj, current_layer, name) {
  out <- ggswim_obj

  appender_list_element <- list(temp = out$layers[[current_layer]]$aes_params)
  names(appender_list_element) <- name

  out$guide_capture <- append(out$guide_capture, values = appender_list_element)
  out
}
