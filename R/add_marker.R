#' @title Add markers of interest swimmer plots
#'
#' @description
#' "Markers" are used to specify events of interest along response trajectories
#' across individual lanes. [add_marker()] wraps [geom_point()] or [geom_label()]
#' depending on the users designation of `color`/`colour` and
#' `label_vals`/`label_names` arguments.
#'
#' See **Aesthetics** for more information.
#'
#' @returns A ggswim object
#'
#' @inheritParams ggplot2::geom_point
#' @param data a dataframe prepared for use with [ggswim()]
#'
#' @section Aesthetics:
#' [add_marker()] understands the following aesthetics (required aesthetics are in bold)
#' when using `color`/`colour` similar to [geom_point()].
#'
#' - **`x`**
#' - **`y`**
#' - **`color`**/**`colour`**
#' - `name` *
#' - `alpha`
#' - `group`
#' - `shape`
#' - `size`
#' - `stroke`
#'
#' [add_marker()] understands the following aesthetics (required aesthetics are in bold)
#' when using `label_vals`/`label_names` similar to [geom_label()]. See "Notes" below for
#' additional considerations and requirements.
#'
#' - **`x`**
#' - **`y`**
#' - **label_vals**
#' - **`label_names`** *
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
#' - If using a static/non-mapping `color` specifier, a mapping `name` is required
#' for aesthetic mapping to render the legend correctly.
#' - If using labels, both `label_vals` and `label_names` are required for
#' proper legend population. At minimum, `label_vals` is needed for data
#' display. These are unique parameter options for [aes()] to ggswim.
#'
#' @export
#'
#' @examples
#'
#' # markers with points and aesthetic mapping params
#' add_marker(
#'   data = infusion_events |> dplyr::mutate(infusion = "Infusion"),
#'   mapping = aes(
#'     x = time_from_initial_infusion,
#'     y = pt_id,
#'     color = infusion
#'   ),
#'   size = 5
#' )
#'
#' # markers with points and static params
#'
#' initial_infusions <- infusion_events |>
#'   dplyr::filter(time_from_initial_infusion == 0)
#'
#' add_marker(
#'   data = initial_infusions,
#'   mapping = aes(
#'     x = time_from_initial_infusion,
#'     y = pt_id,
#'     name = "Initial Infusion"
#'   ),
#'   color = "red",
#'   size = 5
#' )
#'
#' # markers with labels
#' add_marker(
#'   data = end_study_events,
#'   mapping = aes(
#'     y = pt_id, x = time_from_initial_infusion,
#'     label_names = end_study_name,
#'     label_vals = end_study_label
#'   ),
#'   label.size = NA, fill = NA, size = 5
#' )
add_marker <- function(
    mapping = aes(),
    data = NULL,
    ...) {
  # Enforce checks ----
  check_supported_mapping_aes(
    mapping = mapping,
    unsupported_aes = "fill",
    parent_func = "add_marker()"
  )

  check_marker_label_aes(mapping = mapping)

  check_missing_params(
    mapping = mapping,
    params = c("x", "y"),
    parent_func = "add_marker()"
  )

  # Identify labels ----
  has_labels <- "label_vals" %in% names(mapping)

  # Apply geom_label() or geom_point() ----
  if (has_labels) {
    # Convert label mapping params to linked standard params for intuitive API
    names(mapping)[names(mapping) == "label_vals"] <- "label"
    names(mapping)[names(mapping) == "label_names"] <- "colour"

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

      static_colours <- if ("color" %in% names(rlang::dots_list(...))) {
        rlang::dots_list(...)$color
      } else {
        rlang::dots_list(...)$colour
      }

      dots[names(dots) %in% c("color", "colour")] <- c()
    }

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
