#' @title Apply ggswim fixes and display updates
#'
#' @description
#' This function seeks to correct the output of manual overrides introduced by
#' `add_marker()` depending on the combination of layer types the user provides.
#'
#' It is to be run automatically in the background as a print method via
#' `print.ggswim_obj()`.
#'
#' @details
#' In its current state, `build_ggswim()` can only work with a pre-rendered
#' ggswim plot object, therefore it cannot be added to the `+` operator chain.
#'
#' `build_ggswim()` makes use of `ggplot2::guides()` to dynamically override
#' displays in the layers of the ggswim legend. It also applies a call to
#' `ggplot2::scale_color_manual()` in applicable cases where a user calls out
#' a static `color`/`colour` argument in addition to the required `color`
#' mapping aesthetic (handled by arg: `name`).
#'
#' @param ggswim_obj A ggswim object
#'
#' @returns A ggswim object
#' @export
#'
#' @examples
#' ggswim_obj <- ggswim(
#'   data = patient_data,
#'   mapping = aes(
#'     x = delta_t0_months,
#'     y = pt_id,
#'     fill = disease_assessment_status
#'   )
#' )
#' build_ggswim(ggswim_obj)
build_ggswim <- function(ggswim_obj) {
  # Checks ----
  check_ggswim_obj(ggswim_obj)

  # Set up initial capture variables ----
  # Indices for layer positions in ggswim_obj
  label_layer_indices <- c()
  point_layer_indices <- c()
  # Layer data for ggswim_obj data layer capture
  label_layer_data <- data.frame()
  point_layer_data <- data.frame()

  # static_colours for static color manipulation and legend re-definition
  static_colours <- list()

  # Determine indices of layers in ggplot object that contain labels, points, and static colors
  for (i in seq_along(ggswim_obj$layers)) {
    # Check for swim_class attrs, required to allow for other types of layer additions (ex: geom_vline)
    if ("swim_class" %in% names(attributes(ggswim_obj$layers[[i]]))) {
      if (attributes(ggswim_obj$layers[[i]])$swim_class == "marker_label") {
        label_layer_indices <- c(label_layer_indices, i)
      }

      if (attributes(ggswim_obj$layers[[i]])$swim_class == "marker_point") {
        point_layer_indices <- c(point_layer_indices, i)
      }
    }

    if (!is.null(ggswim_obj$layers[[i]]$static_colours)) {
      static_colours$indices <- c(static_colours$indices, i)
      static_colours$colors <- c(static_colours$colors, ggswim_obj$layers[[i]]$static_colours)
      static_colours$name <- c(
        static_colours$name,
        ggswim_obj$layers[[i]]$mapping$colour |>
          get_expr() |>
          as.character()
      )
    }
  }

  # Convert static_colours to a dataframe (will always have equal col lengths)
  static_colours <- data.frame(static_colours)

  # If no `add_marker()` calls, then no need to build legend, exiting early ----
  # requires indices to be built first for determination
  if (rlang::is_empty(label_layer_indices) && rlang::is_empty(point_layer_indices)) {
    # remove ggswim class, so default ggplot2 print methods will take over
    return(
      ggswim_obj |>
        structure(class = class(ggswim_obj) |> setdiff("ggswim_obj"))
    )
  }

  # Create bound layer dataframes ----
  label_layer_data <- bind_layer_data(ggswim_obj,
    layer_indices = label_layer_indices,
    layer_data = label_layer_data
  )

  point_layer_data <- bind_layer_data(ggswim_obj,
    layer_indices = point_layer_indices,
    layer_data = point_layer_data,
    static_colours = static_colours
  )

  override <- get_overrides(ggswim_obj, label_layer_data, point_layer_data)

  # Return fixed ggswim object and guide overrides -----
  (ggswim_obj +
    scale_color_manual(values = setNames(
      override$colour$colour,
      override$colour$colour_mapping
    )) +
    guides(
      shape = override$shape,
      colour = guide_legend(
        override.aes = list(
          label = override$colour$label,
          fill = override$colour$fill,
          color = override$colour$colour,
          shape = override$colour$shape
        )
      )
    )) |>
    # remove ggswim class, so default ggplot2 print methods will take over
    structure(class = class(ggswim_obj) |> setdiff("ggswim_obj"))
}


#' @title Bind layer dataframes for legend
#'
#' @description
#' Internal helper function that returns layer data from `get_layer_data()`
#' as a bound dataframe to help with legend guide definitions.
#'
#' @returns A dataframe
#'
#' @param ggswim_obj description
#' @param layer_indices description
#' @param layer_data description
#' @param static_colours description
#'
#' @keywords internal

bind_layer_data <- function(ggswim_obj, layer_indices, layer_data, static_colours = NULL) {
  for (i in layer_indices) {
    # If first layer, overwrite empty variable
    if (is_empty(layer_data)) {
      layer_data <- get_layer_data(
        data = if (
          # Handle instances where add_marker() inherits data from ggswim()
          is_empty(ggswim_obj$layers[[i]]$data)
        ) {
          ggswim_obj$data
        } else {
          ggswim_obj$layers[[i]]$data
        },
        mapping = ggswim_obj$layers[[i]]$mapping,
        i = i,
        static_colours = static_colours
      )
    } else {
      added_layer_data <- get_layer_data(
        data = ggswim_obj$layers[[i]]$data,
        mapping = ggswim_obj$layers[[i]]$mapping,
        i = i,
        static_colours = static_colours
      )

      layer_data <- bind_rows(layer_data, added_layer_data)
    }
  }

  layer_data
}

#' @title Get overrides list for `guides()`
#'
#' @description
#' Creates a list of override definitions to pass to `guides()` `override.aes`.
#'
#' @param ggswim_obj A ggswim object
#' @param label_layer_data description
#' @param point_layer_data description
#'
#' @returns a list
#'
#' @keywords internal

get_overrides <- function(ggswim_obj,
                          label_layer_data,
                          point_layer_data){
  # TODO: Verify all acceptable column names
  accepted_colour_columns <- c(
    "colour", "label", "group", "fill", "size", "shape", "stroke", "colour_mapping"
  )

  override <- list()

  # Define override aesthetic guides
  override$colour <- bind_rows(label_layer_data, point_layer_data) |>
    select(any_of(accepted_colour_columns))

  if ("colour_mapping" %in% names(override$colour)) {
    # Convert to factor to handle issue with lowercase/uppercase arrange issues
    override$colour$colour_mapping <- factor(override$colour$colour_mapping)
    # Arrange necessary to follow order of ggplot legend outputs
    # (i.e. alphabetical, numeric, etc.)

    # Reference level ordering in underlying layers
    ref_guide <- get_guide_data(ggswim_obj, "color") |>
      mutate(.label = factor(.data$.label, ordered = TRUE))

    override$colour <- override$colour |>
      select(-dplyr::matches("group")) |> # Implemented due to NA vals with inherited add_marker() data
      unique() |>
      # Ensure proper level ordering in output
      mutate(order_col = match(.data$colour_mapping, ref_guide$.label)) |>
      arrange(.data$order_col) |>
      select(-"order_col")
  }

  # Setup label coercion into color layer of legend
  if ("label" %in% names(override$colour)) {
    override$colour$label[is.na(override$colour$label)] <- ""
  }

  override$shape <- "none" # TODO: Determine if default should always be removal

  override
}
