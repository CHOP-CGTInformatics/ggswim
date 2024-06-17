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
