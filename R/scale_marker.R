#' @title Position scales for discrete marker data
#'
#' @description
#' [scale_marker_discrete()] is used to set discrete x aesthetics for swimmer plot
#' markers.
#'
#' @param glyphs Marker glyphs passed to the marker layer, taking on the form of
#' emojis or ASCII shapes. Default glyphs are provided in the absence of defined
#' ones.
#' @param colors Marker colors passed to the marker layer for non-emojis.
#' Default colors are provided in the absence of defined ones.
#' @inheritParams ggplot2::scale_x_discrete
#'
#' @export

scale_marker_discrete <- function(glyphs = NULL, colors = NULL, limits = NULL, ...) {
  markers <- data.frame(glyphs = glyphs, colors = colors, labels = limits) |>
    distinct()

  palette <- pal_markers(
    glyphs = markers$glyphs,
    colors = markers$colors,
    n_values = nrow(markers)
  )

  discrete_scale("marker", rlang::missing_arg(),
    palette = palette,
    limits = markers$labels, ..., na.translate = FALSE
  )
}

#' @noRd
#' @keywords internal
pal_markers <- function(glyphs = NULL, colors = NULL, n_values = NULL) {
  # Define colour and glyph lengths via markers supplied or default values
  n_values <- n_values %||% max(length(glyphs), length(colors))
  if (n_values == 0) n_values <- length(.default_glyphs)

  # Create a vctrs list to store colour and glyph values
  markers <- vctrs::new_rcrd(
    list(
      colour = rep(colors %||% .default_colours, length.out = n_values),
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
#'
#' @export
# Set default glyphs and colours
.default_glyphs <- c("●", "■", "▲", "⬥", "▼", "▢", "□", "△", "◇")

#' @rdname dot-default_glyphs
#' @export
.default_colours <- scales::brewer_pal(palette = "Set1")(9) # 9 is max for brewer
