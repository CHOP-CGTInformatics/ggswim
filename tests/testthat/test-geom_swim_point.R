test_that("geom_swim_point is the right class", {
  layer <- geom_swim_point(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      color = infusion_type
    )
  )

  expected_classes <- c(
    "swim_point", "LayerInstance", "Layer", "ggproto", "gg"
  )


  expect_setequal(attr(layer, "class"), expected_classes)
})

test_that("all expected attributes exist in geom_swim_point", {
  layer <- geom_swim_point(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      color = infusion_type
    )
  )

  attrs <- names(attributes(layer))

  expected_attrs <- c(
    "class",
    "swim_class"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_point works with inherited data", {
  p <- infusion_events |>
    ggplot(mapping = aes(x = time_from_initial_infusion, y = pt_id, colour = infusion_type)) +
    geom_swim_point()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_point works with inherited data and params",
    fig = p
  )
})
