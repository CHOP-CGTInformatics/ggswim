test_that("print.ggswim_layer works with empty mapping layers", {
  label_layer_output <- capture.output(geom_swim_label())
  expected_label_layer_output <- c(
    "geom_swim_label: na.rm = FALSE, parse = FALSE, label.padding = 0.25, label.r = 0.15, label.size = 0.25, size.unit = mm ", #nolint line_length_linter
    "stat_identity: na.rm = FALSE ",
    "position_identity "
  )

  expect_equal(label_layer_output, expected_label_layer_output)

  point_layer_output <- capture.output(geom_swim_point())
  expected_point_layer_output <- c(
    "geom_swim_point: na.rm = FALSE ",
    "stat_identity: na.rm = FALSE ",
    "position_identity "
  )

  expect_equal(point_layer_output, expected_point_layer_output)

  lane_layer_output <- capture.output(geom_swim_lane())
  expected_lane_layer_output <- c(
    "geom_swim_lane: na.rm = FALSE, lineend = butt, linejoin = round ",
    "stat_identity: na.rm = FALSE ",
    "position_identity "
  )

  expect_equal(lane_layer_output, expected_lane_layer_output)
})

test_that("print.ggswim_layer works with established mapping data layers", {
  lane_layer_output <- capture.output(
    geom_swim_lane(
      data = patient_data,
      mapping = aes(x = start_time, y = pt_id, colour = disease_assessment)
    )
  )

  expected_lane_layer_output <- c(
    "mapping x = ~start_time, y = ~pt_id, colour = ~disease_assessment ",
    "geom_swim_lane: na.rm = FALSE, lineend = butt, linejoin = round ",
    "stat_identity: na.rm = FALSE ",
    "position_identity "
  )

  expect_equal(lane_layer_output, expected_lane_layer_output)
})
