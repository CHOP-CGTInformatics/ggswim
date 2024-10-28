test_that("scale_marker_discrete works with provided glyphs and colours", {
  glyphs <- c("●", "■", "▲")
  colours <- c("red", "green", "blue")
  limits <- c("Event1", "Event2", "Event3")

  scale <- scale_marker_discrete(glyphs = glyphs, colours = colours, limits = limits)

  expect_equal(scale$aesthetics, "marker")
  expect_true(is.function(scale$palette))
  expect_equal(length(scale$palette(3)), 3)
})

test_that("scale_marker_discrete uses default glyphs and colours when none are provided", {
  scale <- scale_marker_discrete()

  expect_equal(length(.default_glyphs), 9) # Check default glyph length
  expect_equal(length(.default_colours), 9) # Check default colour length
  expect_true(is.function(scale$palette))
  expect_equal(length(scale$palette(9)), 9)
})

test_that("scale_marker_discrete handles NULL inputs", {
  scale <- scale_marker_discrete(glyphs = NULL, colours = NULL, limits = NULL)

  expect_true(is.function(scale$palette))
  expect_equal(length(scale$palette(9)), 9) # Default palette length when NULL is passed
})

test_that("scale_marker_discrete applies limits correctly", {
  glyphs <- c("●", "■", "▲")
  colours <- c("red", "green", "blue")
  limits <- c("A", "B", "C")

  scale <- scale_marker_discrete(glyphs = glyphs, colours = colours, limits = limits)

  expect_equal(scale$limits, limits)
})

test_that("scale_marker_discrete handles missing values in markers", {
  glyphs <- c("●", NA, "▲")
  colours <- c("red", "green", NA)
  limits <- c("A", "B", "C")

  scale <- scale_marker_discrete(glyphs = glyphs, colours = colours, limits = limits)

  expect_true(is.function(scale$palette))
  # Ensure that the palette still generates values for the valid markers
  expect_equal(length(scale$palette(3)), 3)
})

test_that("scale_marker_discrete throws warning for continuous data types", {
  expect_warning(
    scale_marker_discrete(
      glyphs = 1:3,
      colours = c("red", "green", "blue"),
      limits = c(1:3)
    ),
    "Continuous limits supplied to discrete scale."
  )
})

test_that("scale_marker_discrete warns when more markers are requested than available", {
  glyphs <- c("●", "■", "▲")
  colours <- c("red", "green", "blue")
  limits <- c("1", "2", "3")

  scale <- scale_marker_discrete(
    glyphs = glyphs,
    colours = colours,
    limits = limits
  )

  expect_error(scale$palette(5), "Can't subset elements past the end.") |>
    suppressWarnings()
})

test_that("scale_marker_discrete uses default glyphs when missing", {
  colours <- c("red", "green", "blue")
  limits <- c("A", "B", "C")

  scale <- scale_marker_discrete(glyphs = NULL, colours = colours, limits = limits)

  expect_equal(length(scale$palette(3)), 3)
  expect_equal(vctrs::field(scale$palette(3), "glyphs"), .default_glyphs[1:3]) # First 3 default glyphs
})

test_that("scale_marker_discrete uses default colours when missing", {
  glyphs <- c("●", "■", "▲")
  limits <- c("A", "B", "C")

  scale <- scale_marker_discrete(glyphs = glyphs, colours = NULL, limits = limits)

  expect_equal(length(scale$palette(3)), 3)
  expect_equal(vctrs::field(scale$palette(3), "colour"), .default_colours[1:3]) # First 3 default glyphs
})

test_that("format.marker works", {
  out <- format.marker(x = vctrs::new_rcrd(list(colour = "red", glyphs = "test"),
                                           class = "marker"))

  expect_equal(out, "Glyph: test, Colour: red")
  expect_equal(class(out), "character")
})
