test_that("geom_swim_lane is the right class", {
  layer <- geom_swim_lane(
    data = patient_data,
    mapping = aes(
      x = start_time, y = pt_id,
      xend = end_time, color = disease_assessment
    )
  )

  expected_classes <- c(
    "swim_lane", "LayerInstance", "Layer", "ggproto", "gg"
  )

  expect_setequal(attr(layer, "class"), expected_classes)
})

test_that("all expected attributes exist in geom_swim_lane", {
  layer <- geom_swim_lane(
    data = patient_data,
    mapping = aes(
      x = start_time, y = pt_id,
      xend = end_time, color = disease_assessment
    )
  )

  attrs <- names(attributes(layer))

  expected_attrs <- c(
    "class",
    "swim_class"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_lane works when inheriting data and associated params", {
  p <- patient_data |>
    ggplot(mapping = aes(
      x = start_time, y = pt_id, xend = end_time,
      colour = disease_assessment
    )) +
    geom_swim_lane()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane works with inherited data and params",
    fig = p
  )
})
