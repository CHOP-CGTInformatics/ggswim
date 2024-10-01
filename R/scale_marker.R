#' @title Position scales for discrete marker data
#'
#' @description
#' [scale_marker_discrete()] is used to set discrete x aesthetics for swimmer plot
#' markers.
#'
#' @param glyphs Marker glyphs passed to the marker layer, taking on the form of
#' emojis or ASCII shapes. Default glyphs are provided in the absence of defined
#' ones.
#' @param colours Marker colours passed to the marker layer for non-emojis.
#' Default colours are provided in the absence of defined ones.
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritParams ggplot2::discrete_scale
#'
#' @examples
#' \dontrun{
#' all_events <- dplyr::bind_rows(
#'   infusion_events,
#'   end_study_events
#' )
#'
#' ggplot2::ggplot() +
#'   geom_swim_marker(
#'     data = all_events,
#'     aes(x = time_from_initial_infusion, y = pt_id, marker = label),
#'     size = 5
#'   ) +
#'   scale_marker_discrete(
#'     glyphs = all_events$glyph,
#'     colours = all_events$colour,
#'     limits = all_events$label
#'   )
#'}
#'
#' @export

scale_marker_discrete <- function(glyphs = NULL, colours = NULL, limits = NULL, ...) {
  # Define max value lengths for core params
  n_values <- max(c(length(glyphs), length(colours), length(limits)))

  # If no params assigned (default when using geom_swim_marker), return empty
  # Else, intelligently create equal length core vectors
  if (n_values == 0) {
    markers <- data.frame()
  } else {
    markers <- data.frame(glyphs = glyphs %||% .default_glyphs[0:n_values],
                          colours = colours %||% .default_colours[0:n_values],
                          labels = limits %||% .default_limits[0:n_values]) |>
      distinct()
  }

  palette <- pal_markers(
    glyphs = markers$glyphs,
    colours = markers$colours,
    n_values = nrow(markers)
  )

  discrete_scale("marker", rlang::missing_arg(),
    palette = palette,
    limits = markers$labels, ..., na.translate = FALSE
  )
}

#' @noRd
#' @keywords internal
pal_markers <- function(glyphs = NULL, colours = NULL, n_values = NULL) {
  # Define colour and glyph lengths via markers supplied or default values
  n_values <- n_values %||% max(length(glyphs), length(colours))
  if (n_values == 0) n_values <- length(.default_glyphs)

  # Create a vctrs list to store colour and glyph values
  markers <- vctrs::new_rcrd(
    list(
      colour = rep(colours %||% .default_colours, length.out = n_values),
      glyphs = rep(glyphs %||% .default_glyphs, length.out = n_values)
    ),
    class = "marker"
  )

  # If the values supplied are of a length greater than the default palette,
  # throw a warning.
  function(n) {
    if (n > n_values) {
      cli::cli_warn(
        "This palette can handle a maximum of {n_values} values. \\
         You have supplied {n}."
      )
    }
    markers[seq_len(n)]
  }
}

#' @title ggswim marker defaults
#'
#' @examples
#' ggswim::.default_glyphs
#' ggswim::.default_colours
#' ggswim::.default_limits
#'
#' @export
# Set default glyphs and colours
.default_glyphs <- c("●", "■", "▲", "⬥", "▼", "▢", "□", "△", "◇")

#' @rdname dot-default_glyphs
#' @export
.default_colours <- scales::brewer_pal(palette = "Set1")(9) # 9 is max for brewer

#' @rdname dot-default_glyphs
#' @export
.default_limits <- c("val1", "val2", "val3", "val4", "val5", "val6", "val7", "val8", "val9")
