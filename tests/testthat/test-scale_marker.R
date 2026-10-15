marker_plot <- function(data, scale = scale_marker_discrete(), ...) {
  ggplot2::ggplot(data, ggplot2::aes(x = .data$x, y = .data$id, marker = .data$event)) +
    geom_swim_marker(...) +
    scale
}

test_that("marker scale assigns default styles to observed categories", {
  data <- data.frame(
    x = 1:9,
    id = letters[1:9],
    event = LETTERS[1:9]
  )

  built <- ggplot2::ggplot_build(marker_plot(data))
  marker <- built$data[[1]]$marker

  expect_equal(vctrs::field(marker, "glyphs"), .default_glyphs)
  expect_equal(vctrs::field(marker, "colour"), .default_colours)
})

test_that("bare marker geom discovers the automatic default scale", {
  data <- data.frame(
    x = 1:2,
    id = c("A", "B"),
    event = c("Response", "Review")
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$id, marker = .data$event)
  ) + geom_swim_marker()

  marker <- ggplot2::ggplot_build(plot)$data[[1]]$marker

  expect_equal(vctrs::field(marker, "glyphs"), .default_glyphs[1:2])
  expect_equal(vctrs::field(marker, "colour"), .default_colours[1:2])
})

test_that("marker palette fills in glyphs or colours when one style is omitted", {
  colour_only <- scale_marker_discrete(
    colours = c("red", "blue"), limits = c("A", "B")
  )$palette(2)
  glyph_only <- scale_marker_discrete(
    glyphs = c("+", "x"), limits = c("A", "B")
  )$palette(2)

  expect_equal(vctrs::field(colour_only, "glyphs"), .default_glyphs[1:2])
  expect_equal(vctrs::field(colour_only, "colour"), c("red", "blue"))
  expect_equal(vctrs::field(glyph_only, "glyphs"), c("+", "x"))
  expect_equal(vctrs::field(glyph_only, "colour"), .default_colours[1:2])
})

test_that("marker palette preserves missing values in configured style records", {
  styles <- scale_marker_discrete(
    glyphs = c("●", NA, "▲"),
    colours = c("red", "green", NA),
    limits = c("A", "B", "C")
  )$palette(3)

  expect_equal(vctrs::field(styles, "glyphs"), c("●", NA, "▲"))
  expect_equal(vctrs::field(styles, "colour"), c("red", "green", NA))
})

test_that("marker scale errors when categories exhaust missing defaults", {
  limits <- paste0("category", seq_len(10))

  expect_error(
    scale_marker_discrete(limits = limits),
    "10 categories requested, but only 9 default glyphs are available"
  )
  expect_error(
    scale_marker_discrete(glyphs = paste0("g", seq_len(10)), limits = limits),
    "10 categories requested, but only 9 default colours are available"
  )
  expect_error(
    scale_marker_discrete(colours = grDevices::gray.colors(10), limits = limits),
    "10 categories requested, but only 9 default glyphs are available"
  )
})

test_that("marker scale accepts ten categories with explicit glyphs and colours", {
  limits <- paste0("category", seq_len(10))
  glyphs <- paste0("g", seq_len(10))
  glyphs[10] <- NA_character_
  colours <- grDevices::gray.colors(10)
  scale <- scale_marker_discrete(glyphs, colours, limits)

  expect_equal(scale$limits, limits)
  styles <- scale$palette(10)
  expect_equal(vctrs::field(styles, "glyphs"), glyphs)
  expect_equal(vctrs::field(styles, "colour"), colours)
})

test_that("marker scale errors when more categories request styles than configured", {
  scale <- scale_marker_discrete(
    glyphs = c("●", "■"), colours = c("red", "blue"),
    limits = c("A", "B")
  )

  expect_error(scale$palette(3), "3 categories requested, but only 2 styles")
})

test_that("marker scale drops unused factor levels and follows explicit limits", {
  data <- data.frame(
    x = 1:2,
    id = letters[1:2],
    event = factor(c("B", "A"), levels = c("unused", "B", "A"))
  )
  scale <- scale_marker_discrete(
    glyphs = c("+", "x"),
    colours = c("red", "blue"),
    limits = c("A", "B")
  )

  built <- ggplot2::ggplot_build(marker_plot(data, scale))
  marker <- built$data[[1]]$marker

  expect_equal(
    built$plot$scales$get_scales("marker")$get_limits(),
    c("A", "B")
  )
  expect_equal(vctrs::field(marker, "glyphs"), c("x", "+"))
  expect_equal(vctrs::field(marker, "colour"), c("blue", "red"))
})

test_that("marker scale keeps limit order separate from displayed labels", {
  data <- data.frame(x = 1:2, id = letters[1:2], event = c("A", "B"))
  scale <- scale_marker_discrete(
    glyphs = c("+", "x"),
    colours = c("red", "blue"),
    limits = c("B", "A"),
    labels = c(A = "Alpha", B = "Beta")
  )

  built <- ggplot2::ggplot_build(marker_plot(data, scale))
  marker <- built$data[[1]]$marker
  trained <- built$plot$scales$get_scales("marker")

  expect_equal(vctrs::field(marker, "glyphs"), c("x", "+"))
  expect_equal(as.character(trained$get_labels()), c("Beta", "Alpha"))
})

test_that("missing and out-of-limit marker values are not drawn", {
  data <- data.frame(
    x = 1:3,
    id = letters[1:3],
    event = c("A", NA, "outside")
  )
  scale <- scale_marker_discrete(
    glyphs = "+", colours = "red", limits = "A"
  )

  built <- ggplot2::ggplot_build(marker_plot(data, scale))

  expect_equal(
    vctrs::vec_detect_missing(built$data[[1]]$marker),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("explicit marker styles stay associated with categories after factor reordering", {
  data <- data.frame(
    x = 1:2,
    id = letters[1:2],
    event = factor(c("A", "B"), levels = c("B", "A"))
  )
  scale <- scale_marker_discrete(
    glyphs = c("+", "x"),
    colours = c("red", "blue"),
    limits = c("A", "B")
  )

  built <- ggplot2::ggplot_build(marker_plot(data, scale))
  marker <- built$data[[1]]$marker

  expect_equal(vctrs::field(marker, "glyphs"), c("+", "x"))
  expect_equal(vctrs::field(marker, "colour"), c("red", "blue"))
})

test_that("scale_marker_discrete warns for continuous limits", {
  expect_warning(
    scale_marker_discrete(
      glyphs = 1:3,
      colours = c("red", "green", "blue"),
      limits = 1:3
    ),
    "Continuous limits supplied to discrete scale."
  )
})

test_that("format.marker works", {
  out <- format.marker(
    x = vctrs::new_rcrd(list(colour = "red", glyphs = "test"), class = "marker")
  )

  expect_equal(out, "Glyph: test, Colour: red")
  expect_equal(class(out), "character")
})
