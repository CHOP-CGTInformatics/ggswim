test_that("geom_swim_marker is the right class", {
  layer <- geom_swim_marker(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      marker = label
    )
  )

  expected_classes <- c(
    "LayerInstance", "Layer", "ggproto", "gg"
  )

  expect_setequal(attr(layer, "class"), expected_classes)
})

test_that("all expected attributes exist in geom_swim_lane", {
  layer <- geom_swim_marker(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      marker = label
    )
  )

  attrs <- names(attributes(layer))

  expected_attrs <- c(
    "class"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_marker works when inheriting data and associated params", {
  p <- ggplot(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      marker = label
    )
  ) +
    geom_swim_marker()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_marker inherited data and params",
    fig = p
  )
})

test_that("geom_swim_lane works when inheriting data and associated params", {
  p <- ggplot(
    data = patient_data,
    mapping = aes(
      x = start_time, xend = end_time, y = pt_id, colour = disease_assessment
    )
  ) +
    geom_swim_lane()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane inherited data and params",
    fig = p
  )
})
