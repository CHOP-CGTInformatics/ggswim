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
#' More information about accepted mapping arguments can be found in **Aesthetics**.
#' @param ... Other arguments passed to `add_marker()`, often aesthetic fixed values,
#' i.e. `color = "red"` or `size = 3`.
#'
#' @section Aesthetics:
#' `add_marker()` understands the following aesthetics (required aesthetics are in bold)
#' when using `color`/`colour` akin to `geom_point()`
#'
#' - **`x`**
#' - **`y`**
#' - **`color`**/**`colour`**
#' - `alpha`
#' - `group`
#' - `shape`
#' - `size`
#' - `stroke`
#'
#' `add_marker()` understands the following aesthetics (required aesthetics are in bold)
#' when using `label` akin to `geom_label()`. See "Notes" below for additional
#' considerations and requirements.
#'
#' - **`x`**
#' - **`y`**
#' - **`color`**/**`colour`**
#' - **`label`** *
#' - `name` **
#' - `alpha`
#' - `angle`
#' - `family`
#' - `fontface`
#' - `group`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#'
#' **Notes**:
#'
#' - `add_marker()` **does not** support mapping using `fill`.
#' - *If using `label`, a secondary `color` reference is required to make
#' the label appear in the color layer of the legend.
#' - **If using a dynamic `color` call, a specifying `name` is required for
#' aesthetic mapping to render the legend correctly.
#'
#' @export

add_marker <- function(
    data = NULL,
    mapping = aes(),
    ...
) {

  # Enforce checks ----
  check_supported_mapping_aes(mapping = mapping,
                              unsupported_aes = c("fill"),
                              parent_func = "add_marker()")

  # Identify labels ----
  labels <- ifelse("label" %in% names(mapping), TRUE, FALSE)

  # Apply geom_label() or geom_point() ----
  if (labels) {
    out <- geom_label(
      data = data,
      mapping = mapping,
      ...
    )

    # Tag the layer with a reference attribute
    attributes(out)$swim_class <- "marker_label"

  } else {

    dots <- rlang::dots_list(...)
    static_colours <- NULL
    name_detected <- "name" %in% names(mapping)

    # Artificially create a column that will serve as the aes mapping "color" column
    # Currently dependent on "name" in mapping aes
    if (name_detected) {
      data <- data |>
        dplyr::mutate(!!mapping$name := mapping$name)

      mapping$colour <- rlang::sym(mapping$name)
      mapping <- mapping[names(mapping) != "name"] # remove name

      static_colours <- if ("color" %in% names(rlang::dots_list(...))){
        rlang::dots_list(...)$color
      } else {
        rlang::dots_list(...)$colour
      }

      dots[names(dots) %in% c("color" , "colour")] <- c()
    }

    # TODO: Fix issue causing removal of NA values and subsequent `build_ggswim()` failure
    out <- geom_point(
      data = data,
      mapping = mapping,
      colour = dots[names(dots) %in% c("color", "colour")],
      na.rm = TRUE,
      ...
    )

    if (is.null(dots$colour) && is.null(dots$color)) {
        out$aes_params$colour <- NULL
    }

    out$static_colours <- static_colours

    # Tag the layer with a reference attribute
    attributes(out)$swim_class <- "marker_point"
  }

  out
}
