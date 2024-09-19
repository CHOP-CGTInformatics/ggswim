

scale_marker_discrete <- function(glyphs = NULL, colors = NULL, limits = NULL, ...) {

  markers <- data.frame(glyphs = glyphs, colors = colors, labels = limits) |>
    distinct()

  palette <- pal_markers(
    glyphs = markers$glyphs,
    colors = markers$colors,
    n_values = nrow(markers)
  )

  discrete_scale("marker", rlang::missing_arg(), palette = palette,
                 limits = markers$labels, ..., na.translate = FALSE)
}


.default_glyphs <- c("⛔", "✔", "✖", "☀", "☢", "☠", "☮", "⚛", "♥",
                     "✀")

pal_markers <- function(glyphs = NULL, colors = NULL, n_values = NULL) {

  n_values <- n_values %||% max(length(glyphs), length(colors))
  if (n_values == 0) n_values <- length(.default_glyphs)

  markers <- vctrs::new_rcrd(
    list(
      colour = rep(colors %||% "black", length.out = n_values),
      glyphs = rep(glyphs %||% .default_glyphs, length.out = n_values)
    ),
    class = "marker"
  )

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
