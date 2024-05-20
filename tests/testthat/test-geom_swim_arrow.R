test_that("geom_swim_arrow is the right class", {
  arrow_data <- sample_arrow_data()

  layer <- geom_swim_arrow(
    data = arrow_data,
    mapping = aes(xend = end_time, y = pt_id),
    linewidth = .1,
    arrow_neck_length = 5,
    arrow_head_length = unit(0.25, "inches"),
    arrow_colour = "slateblue",
    arrow_fill = "cyan"
  )

  expect_equal(attr(layer, "class"), "swim_arrow")
})

test_that("all expected attributes exist in geom_swim_arrow", {
  arrow_data <- sample_arrow_data()

  layer <- geom_swim_arrow(
    data = arrow_data,
    mapping = aes(xend = end_time, y = pt_id),
    linewidth = .1,
    arrow_neck_length = 5,
    arrow_head_length = unit(0.25, "inches"),
    arrow_colour = "slateblue",
    arrow_fill = "cyan"
  )

  attrs <- names(attributes(layer))

  expected_attrs <- c(
    "class",
    "stat",
    "position",
    "mapping",
    "data",
    "show.legend",
    "inherit.aes",
    "params",
    "arrow_colour",
    "arrow_head_length",
    "arrow_neck_length",
    "arrow_type"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_arrow makes expected plot", {
  arrow_data <- sample_arrow_data()

  layer <- geom_swim_arrow(
    data = arrow_data,
    mapping = aes(xend = end_time, y = pt_id),
    linewidth = .1,
    arrow_neck_length = 5,
    arrow_head_length = unit(0.25, "inches"),
    arrow_colour = "slateblue",
    arrow_fill = "cyan"
  )

  p <- simple_plot() + layer

  vdiffr::expect_doppelganger(
    title = "Arrows work with simple plot",
    fig = p
  )
})
