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

  expected_classes <- c(
    "LayerInstance", "Layer", "ggproto", "gg"
  )

  expect_setequal(attr(layer, "class"), expected_classes)
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
    "class"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_arrow makes expected plot", {
  arrow_data <- sample_arrow_data()

  layer <- geom_swim_arrow(
    data = arrow_data,
    mapping = aes(xend = end_time, y = pt_id),
    linewidth = .1,
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

test_that("geom_swim_arrow makes expected plot when inheriting data", {
  arrow_data <- sample_arrow_data()

  p <- arrow_data |>
    ggplot() +
    geom_swim_arrow(
      mapping = aes(xend = end_time, y = pt_id),
      linewidth = .1,
      arrow_head_length = unit(0.25, "inches"),
      arrow_colour = "slateblue",
      arrow_fill = "cyan"
    )

  vdiffr::expect_doppelganger(
    title = "Arrows work with inherited data",
    fig = p
  )
})

test_that("geom_swim_arrow creates a ggswim_obj", {
  arrow_data <- sample_arrow_data()

  layer <- geom_swim_arrow(
    data = arrow_data,
    mapping = aes(xend = end_time, y = pt_id),
    linewidth = .1,
    arrow_head_length = unit(0.25, "inches"),
    arrow_colour = "slateblue",
    arrow_fill = "cyan"
  )

  p <- ggplot2::ggplot() +
    layer

  expect_setequal(class(p), c("gg", "ggplot"))
})
