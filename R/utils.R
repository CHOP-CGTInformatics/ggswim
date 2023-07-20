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
#' standard numeric ggplot 2 shapes, emoji, or unicode form (ex: "\U1F464").
#' Shapes can be supplied as character strings or integers.
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param groups additional specifier to indicate groups, optional. Example:
#' treatment groups or cohorts in a study.
#'
#' @returns a character vector
#'
#' @importFrom ggplot2 ggplot_build
#'
#' @keywords internal

update_gg_legend_order <- function(gg, lanes, markers, groups = NULL) {
  # Make "ggplot_built" object
  gg_obj <- ggplot_build(gg)

  # Grab existing labels in default order
  # existing_legend_labels <- gg_obj$plot$scales$scales[[1]]$get_labels()

  # # Get all desired legend labels in order of lanes > markers
  # legend_label_order <- c(as.vector(lanes), names(markers))
  # # Subset for only those that appear in `existing_legend_labels`
  # legend_label_order <- existing_legend_labels[match(legend_label_order, existing_legend_labels)]
  # # In instances where not all appear, remove NAs
  # legend_label_order <- legend_label_order[!is.na(legend_label_order)]

  fill_legend_labels <- gg_obj$plot$scales$scales[[3]]$get_labels()
  # Get all desired legend labels in order of lanes > markers
  fill_label_order <- as.vector(lanes)
  # Subset for only those that appear in `existing_legend_labels`
  fill_label_order <- fill_legend_labels[match(fill_label_order, fill_legend_labels)]
  # In instances where not all appear, remove NAs
  fill_label_order <- fill_label_order[!is.na(fill_label_order)]

  color_legend_labels <- gg_obj$plot$scales$scales[[4]]$get_labels()
  # Get all desired legend labels in order of lanes > markers
  color_label_order <- c(groups, names(markers))
  # Subset for only those that appear in `existing_legend_labels`
  color_label_order <- color_legend_labels[match(color_label_order, color_legend_labels)]
  # In instances where not all appear, remove NAs
  color_label_order <- color_label_order[!is.na(color_label_order)]

  list(fill_label_order = fill_label_order, color_label_order = color_label_order)
}

#' @title Apply updated ggplot legend order
#'
#' @description
#' This helper function takes the updated legend order from `get_gg_legend_order`
#' and applies the output directly to the labels of a ggplot object's legend.
#'
#' @param gg A `ggplot` object
#' @param markers a named list defining marker events on a `lane` in either
#' standard numeric ggplot 2 shapes, emoji, or unicode form (ex: "\U1F464").
#' Shapes can be supplied as character strings or integers.
#' @param lanes a list of character strings that define the colored line segments
#' for `id`. Colors are supplied by setting list elements equal to hex or named colors.
#' In the absence of colors, default `ggplot2` colors will be supplied.
#' @param groups additional specifier to indicate groups, optional. Example:
#' treatment groups or cohorts in a study.
#'
#' @returns a ggplot object
#'
#' @keywords internal

apply_gg_legend_order <- function(gg, lanes, markers, groups = NULL) {
  gg_obj <- ggplot_build(gg)

  update_legend_order <- update_gg_legend_order(gg, lanes, markers, groups)
  # gg_obj$plot$scales$scales[[1]]$labels <- update_gg_legend_order(gg, lanes, markers, groups)

  gg_obj$plot$scales$scales[[3]]$get_labels() <- update_legend_order$fill_label_order
  gg_obj$plot$scales$scales[[4]]$get_labels() <- update_legend_order$color_label_order

  gg <- gg_obj$plot
  gg
}