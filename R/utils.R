#' @title Build Layer Data
#'
#' @description
#' This internal function looks for color and fill aesthetic mapping data in
#' constructed ggplot objects and appends layer data with associated data from
#' the parent data frame.
#'
#' @returns A dataframe
#'
#' @param data The data responsible for the current layer
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param i An integer to supply for the layer to retrieve. If none given, defaults
#' to `1L`.
#' @param fixed_colours an inherited dataframe from add_marker that captures
#' and defines fixed color indices
#'
#' @keywords internal
get_layer_data <- function(data, mapping, i = 1L, fixed_colours = NULL) {
  layer_data <- NULL

  # Starting with color/colour, since that will always need to be given to result
  # in a legend layer for ggplots
  aes_mapping <- unlist(mapping)

  # TODO: Currently functionality is limited to and requires a color or fill aesthetic
  if (any(c("color", "colour", "color_new", "colour_new") %in% names(aes_mapping))) {
    colour_or_color <- case_when(
      "color" %in% names(aes_mapping) ~ "color",
      "color_new" %in% names(aes_mapping) ~ "color_new",
      "colour_new" %in% names(aes_mapping) ~ "colour_new",
      TRUE ~ "colour"
    )
    colour_mapping_var <- retrieve_original_aes(data, aes_mapping, aes_var = colour_or_color)
    colour_mapping <- data[[colour_mapping_var]]
  } else {
    colour_or_color <- NULL
    colour_mapping <- NULL
  }

  # TODO: Artifact of old setup, currently fill is not used but we likely will undo
  # this and reapply in the future
  if ("fill" %in% names(aes_mapping)) {
    fill_mapping_var <- retrieve_original_aes(data, aes_mapping, aes_var = "fill")
    fill_mapping <- data[[fill_mapping_var]]
  } else {
    fill_mapping <- NULL
  }

  if (!is.null(fill_mapping)) {
    layer_data <- cbind(layer_data(i = i), fill_mapping) |>
      arrange(fill_mapping) # Assume correct since ggplot legend is arranged this way
  }


  if (!is.null(colour_mapping)) {
    layer_data <- cbind(layer_data(i = i), colour_mapping) |>
      arrange(colour_mapping) # Assume correct since ggplot legend is arranged this way

    # Handle fixed_colours
    if (!is.null(fixed_colours)) {
      if (i %in% fixed_colours$indices) {
        layer_data$colour <- fixed_colours$colors[fixed_colours$indices == i]
      }
    }
  }

  layer_data
}

#' @title Retrieve original vars from coerced vars
#'
#' @description
#' Attempt to detect instances where users manipulate `aes()` names that ggswim
#' requires to access and identify layer types downstream. In instances where
#' such a coercion is detected, attempt to retrieve the original `aes()` name.
#'
#' @details
#' ggswim references internal ggplot layers and any aesthetic mapping
#' required for downstream rendering. If a user applies a coercion in a function,
#' ggswim may not be able to parse the original variable. For example, in
#' `ggswim(mtcars, aes(x = hp, y = cyl, color = factor(disp)`),
#' `rlang::get_expr()` will see the color mapping aesthetic as `factor(disp)`,
#' and not `disp`.
#'
#' @param data the data responsible for the current layer
#' @param aes_mapping a list of mapping data (i.e. `unlist(mapping)`)
#' @param aes_var the aesthetic variable to test for (ex: `color`, `shape`)
#'
#' @returns The original variable name as a character string
#'
#' @keywords internal

retrieve_original_aes <- function(data, aes_mapping, aes_var) {
  layer_aes <- aes_mapping[[aes_var]] |>
    get_expr() |>
    paste()
  original_var <- layer_aes[layer_aes %in% names(data)]

  # If original var cannot be validated, throw error
  check_coerced_data(expr = original_var)

  original_var
}
