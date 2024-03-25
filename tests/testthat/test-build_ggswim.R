test_that("get_overrides works", {
  ref_guide <- tibble::tribble(
    ~"colour", ~".value", ~".label", ~"shape",
    "gray", "fill1", "Fill Layer", 15
  )

  label_layer_data <- tibble::tribble(
    ~"colour", ~"label", ~"size", ~"fill", ~"shape", ~"stroke", ~"alpha", ~"colour_mapping",
    "gray", "❌", 5, NA, NA, NA, NA, "layer1"
  )

  point_layer_data <- tibble::tribble(
    ~"colour", ~"size", ~"fill", ~"shape", ~"stroke", ~"alpha", ~"colour_mapping",
    "firebrick", 5, NA, 19, 0.5, 1, "point1"
  )

  expected_out <- list(
    colour = tibble::tribble(
      ~"colour", ~"label", ~"fill", ~"size", ~"shape", ~"stroke", ~"colour_mapping",
      "gray", "❌", NA, 5, NA, NA, "layer1",
      "firebrick", "", NA, 5, 19, 0.5, "point1"
    ),
    shape = "none"
  )

  expected_out$colour$colour_mapping <- as.factor(expected_out$colour$colour_mapping)

  out <- get_overrides(ref_guide, label_layer_data, point_layer_data)

  expect_equal(out, expected_out)
})
