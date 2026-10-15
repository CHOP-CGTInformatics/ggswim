#' @title Add markers to swimmer plots
#'
#' @description
#' Markers are specific symbols or indicators placed on the lanes of a swimmer plot
#' to denote particular events, milestones, or statuses. They provide additional
#' contextual information about significant occurrences during the timeline, such
#' as treatment responses or adverse events.
#'
#' @details
#' Map the event category to `marker`; the category selects a glyph and colour
#' through [scale_marker_discrete()]. The marker scale controls both the plot
#' glyph and its legend key. It sets the marker colour after ggplot2 applies
#' aesthetic scales, so `colour` mappings, fixed `colour` settings, and
#' `after_scale()` colour transformations do not override marker colours. Set
#' marker colours with the scale's `colours` argument instead.
#'
#' If using icon glyphs from [fontawesome] or [bootstrap], first call
#' [load_fonts()] to register the matching fonts. The supported `family` values
#' are:
#'
#' - "Bootstrap"
#' - "FontAwesome-Brands"
#' - "FontAwesome-Regular"
#' - "FontAwesome-Solid"
#'
#' @inheritParams ggplot2::geom_text
#'
#' @section Aesthetics:
#' Required aesthetics are **`x`**, **`y`**, and **`marker`**. `marker` should
#' identify the event category; use [scale_marker_discrete()] to assign glyphs,
#' colours, and legend labels. Other text aesthetics are inherited from
#' [ggplot2::geom_text()].
#'
#' @section Marker alignment:
#' Markers are rendered as text glyphs. Even when a marker and lane share the
#' same y-coordinate, the visible symbol can appear slightly above or below the
#' lane because glyphs occupy different positions within the text reference
#' box. This placement can vary with the font and graphics device, including
#' when emoji or fallback fonts are used. You can adjust `vjust` for a
#' particular plot, though one value may not suit every glyph or font; inspect
#' the exported figure. ggswim retains ggplot2's text-rendering behavior because
#' a correction based on one glyph or font may misalign others.
#'
#' @examples
#' events <- data.frame(
#'   time = c(2, 4),
#'   id = c("A", "B"),
#'   event = c("Response", "Relapse")
#' )
#' ggplot2::ggplot(events, ggplot2::aes(x = time, y = id, marker = event)) +
#'   geom_swim_marker(size = 5) +
#'   scale_marker_discrete(
#'     glyphs = c("+", "x"),
#'     colours = c("steelblue", "firebrick"),
#'     limits = c("Response", "Relapse")
#'   )
#'
#' \dontrun{
#' # The first call downloads fonts into the user cache and needs internet.
#' load_fonts()
#' icon_events <- data.frame(time = 2, id = "A", event = "Review")
#' ggplot2::ggplot() +
#'   geom_swim_marker(
#'     data = icon_events,
#'     mapping = ggplot2::aes(x = time, y = id, marker = event),
#'     family = "FontAwesome-Solid", size = 5
#'   ) +
#'   scale_marker_discrete(
#'     glyphs = fontawesome("fa-check"), limits = "Review"
#'   )
#' }
#'
#' @export

geom_swim_marker <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  check_overlap = FALSE,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSwimMarker,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      check_overlap = check_overlap,
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_swim_marker
#' @format NULL
#' @usage NULL
#' @export
GeomSwimMarker <- ggproto(
  "GeomSwimMarker",
  GeomText,
  default_aes = GeomText$default_aes[names(GeomText$default_aes) != "colour"],
  required_aes = c("x", "y", "marker"),
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    size.unit = "mm",
    check_overlap = FALSE,
    na.rm = FALSE
  ) {
    has_marker_record <- inherits(data$marker, "vctrs_rcrd") &&
      all(c("colour", "glyphs") %in% vctrs::fields(data$marker))

    if (!has_marker_record) {
      cli::cli_abort(c(
        "{.fn geom_swim_marker} requires marker styles from a discrete scale.",
        "i" = "Map a category with {.code aes(marker = category)} and add {.fn scale_marker_discrete}."
      ))
    }

    data$colour <- vctrs::field(data$marker, "colour")
    data$label <- vctrs::field(data$marker, "glyphs")
    data$marker <- NULL

    GeomText$draw_panel(
      data,
      panel_params,
      coord,
      check_overlap = check_overlap,
      size.unit = size.unit,
      na.rm = na.rm
    )
  },
  draw_key = function(data, params, size) {
    data$colour <- vctrs::field(data$marker, "colour")
    data$label <- vctrs::field(data$marker, "glyphs")
    data$marker <- NULL

    draw_key_text(data, params, size)
  }
)
