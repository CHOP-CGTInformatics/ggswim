#' @title Get ggplot legend order
#'
#' @description
#' Helper function to retrieve the existing legend order for a ggplot object
#' and return the order in the order of factor levels and split by `lanes` to
#' `markers`
#'
#' @details
#' Retrieving the existing legend order for a ggplot object involves accessing
#' sub-elements of the object via `ggplot_build()`. In addition to returning
#' the legend order, this function also returns only those that actually
#' make it into the final plot, and not all associated markers defined in the
#' data.
#'
#' @param gg a ggplot object
#' @param markers a named list defining marker events on a `lane` in either
#' standard numeric ggplot 2 shapes, emoji, or unicode form .
#' Shapes can be supplied as character strings or integers.
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#'
#' @returns a character vector
#'
#' @importFrom ggplot2 ggplot_build
#'
#' @keywords internal

update_gg_legend_order <- function(gg, lanes, markers) {
  gg_build <- ggplot_build(gg)$plot$scales$scales

  # Find the index positions where "colour" or "fill" is present
  index_with_colour <- which(sapply(gg_build, function(x) "colour" %in% x$aesthetics))
  index_with_fill <- which(sapply(gg_build, function(x) "fill" %in% x$aesthetics))

  fill_legend_labels <- gg_build[[index_with_fill]]$get_labels()
  # Get all desired legend labels in order of lanes > markers
  fill_label_order <- as.vector(lanes)
  # Subset for only those that appear in `existing_legend_labels`
  fill_label_order <- fill_legend_labels[match(fill_label_order, fill_legend_labels)]
  # In instances where not all appear, remove NAs
  fill_label_order <- fill_label_order[!is.na(fill_label_order)]

  color_legend_labels <- gg_build[[index_with_colour]]$get_labels()
  # Get all desired legend labels in order of lanes > markers
  color_label_order <- names(markers)
  # Subset for only those that appear in `existing_legend_labels`
  color_label_order <- color_legend_labels[match(color_label_order, color_legend_labels)]
  # In instances where not all appear, remove NAs
  color_label_order <- color_label_order[!is.na(color_label_order)]

  list(
    fill_label_order = fill_label_order,
    color_label_order = color_label_order,
    index_with_colour = index_with_colour,
    index_with_fill = index_with_fill
  )
}

#' @title Apply updated ggplot legend order
#'
#' @description
#' This helper function takes the updated legend order from `get_gg_legend_order`
#' and applies the output directly to the labels of a ggplot object's legend.
#'
#' @param gg A `ggplot` object
#' @param markers a named list defining marker events on a `lane` in either
#' standard numeric ggplot 2 shapes, emoji, or unicode form .
#' Shapes can be supplied as character strings or integers.
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#'
#' @returns a ggplot object
#'
#' @keywords internal

apply_gg_legend_order <- function(gg, lanes, markers) {
  gg_obj <- ggplot_build(gg)

  update_legend_order <- update_gg_legend_order(gg, lanes, markers)

  gg_obj$plot$scales$scales[[update_legend_order$index_with_fill]]$labels <- update_legend_order$fill_label_order
  gg_obj$plot$scales$scales[[update_legend_order$index_with_colour]]$labels <- update_legend_order$color_label_order

  gg <- gg_obj$plot
  gg
}

#' @title Detect if object is numeric or numeric-coercible
#'
#' @description
#' This utility function takes a vector or variable and attempts to detect
#' whether it is numeric or numeric-coercible (i.e. a numeric character string).
#'
#' @details
#' At present, this is only intended to be used in detecting coercible character
#' strings, not factors or boolean values since coercion to numeric is both
#' possible and meaningful.
#'
#' @param data a vector or variable to test
#'
#' @importFrom cli cli_abort
#'
#' @returns a boolean
#'

is_numeric_coercible <- function(data) {
  data <- unlist(data)

  valid_data_types <- c("character", "numeric", "integer")

  if (!class(data) %in% valid_data_types) {
    cli_abort(message = c(
      "x" = "{.arg data} not a valid data type to check for numeric coercion. Supplied class: {class(data)}",
      "i" = "Class type should be one of the following: {valid_data_types}"
    ))
  }

  all(!is.na(suppressWarnings(as.numeric(data))))
}
