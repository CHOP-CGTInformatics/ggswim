#' @title Add markers to swimmer plots
#'
#' @description
#' Markers are specific symbols or indicators placed on the lanes of a swimmer plot
#' to denote particular events, milestones, or statuses. They provide additional
#' contextual information about significant occurrences during the timeline, such
#' as treatment responses or adverse events.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @examples
#' all_events <- dplyr::bind_rows(
#'   infusion_events,
#'   end_study_events
#' )
#'
#' ggplot() +
#'   geom_swim_lane(data = patient_data,
#'                  aes(x = start_time, xend = end_time, y = pt_id, colour = disease_assessment)) +
#'   geom_swim_marker(
#'     data = all_events,
#'     aes(x = time_from_initial_infusion, y = pt_id, marker = label),
#'     size = 10
#'   )
#'
#'
#' @export

geom_swim_marker <- function(mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             ...,
                             check_overlap = FALSE,
                             size.unit = "mm",
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
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
  "GeomSwimMarker", GeomText,
  default_aes = GeomText$default_aes[names(GeomText$default_aes) != "colour"],
  required_aes = c("x", "y", "marker"),
  draw_panel = function(self, data, panel_params, coord, size.unit = "mm",
                        check_overlap = FALSE, na.rm = FALSE) {
    data$colour <- vctrs::field(data$marker, "colour")
    data$label <- vctrs::field(data$marker, "glyphs")
    data$marker <- NULL

    GeomText$draw_panel(
      data, panel_params, coord,
      check_overlap = check_overlap,
      size.unit = size.unit, na.rm = na.rm
    )
  },
  draw_key = function(data, params, size) {
    data$colour <- vctrs::field(data$marker, "colour")
    data$label <- vctrs::field(data$marker, "glyphs")
    data$marker <- NULL

    draw_key_text(data, params, size)
  }
)
