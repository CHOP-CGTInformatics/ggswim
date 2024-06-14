test_that("print.ggswim_layer works with empty mapping layers", {
  arrow_layer_output <- capture.output(geom_swim_arrow())
  expected_arrow_layer_output <- c(
    "geom_swim_arrow: na.rm = FALSE, lineend = butt, linejoin = round, arrow = NULL, arrow.fill = NULL ", # nolint line_length_linter
    "stat_identity: na.rm = FALSE ",
    "position_identity "
  )

  expect_equal(arrow_layer_output, expected_arrow_layer_output)
})

test_that("print.ggswim_layer works with established mapping data layers", {
  arrow_data <- patient_data |>
    dplyr::left_join(
      end_study_events |>
        dplyr::select(pt_id, end_study_name),
      by = "pt_id"
    ) |>
    dplyr::select(pt_id, end_time, end_study_name) |>
    dplyr::filter(.by = pt_id, end_time == max(end_time)) |>
    unique()

  arrow_layer_output <- capture.output(
    geom_swim_arrow(
      data = arrow_data,
      mapping = aes(xend = end_time, y = pt_id),
      linewidth = .1,
      arrow_neck_length = 5,
      arrow_head_length = grid::unit(0.25, "inches"),
      arrow_colour = "slateblue",
      arrow_fill = "cyan", position = "stack"
    )
  )

  expected_arrow_layer_output <- c(
    "mapping y = ~pt_id, xend = ~end_time ",
    "geom_swim_arrow: na.rm = FALSE, lineend = butt, linejoin = round, arrow = NULL, arrow.fill = cyan, linewidth = 0.1 ", # nolint line_length_linter
    "stat_identity: na.rm = FALSE ",
    "position_stack "
  )

  expect_equal(arrow_layer_output, expected_arrow_layer_output)
})
