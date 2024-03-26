test_that("get_overrides works", {
  ref_guide <- tibble::tribble(
    ~"colour", ~".value", ~".label", ~"shape",
    "gray", "fill1", "Fill Layer", 15
  )

  label_layer_data <- tibble::tribble(
    ~"colour", ~"label", ~"size", ~"fill", ~"shape", ~"colour_mapping",
    "gray", "❌", 5, NA, NA, "layer1"
  )

  point_layer_data <- tibble::tribble(
    ~"colour", ~"size", ~"fill", ~"shape", ~"colour_mapping",
    "firebrick", 5, NA, 19, "point1"
  )

  expected_out <- list(
    colour = tibble::tribble(
      ~"colour", ~"label", ~"fill", ~"size", ~"shape", ~"colour_mapping",
      "gray", "❌", NA, 5, NA, "layer1",
      "firebrick", "", NA, 5, 19, "point1"
    ),
    shape = "none"
  )

  expected_out$colour$colour_mapping <- as.factor(expected_out$colour$colour_mapping)

  out <- get_overrides(ref_guide, label_layer_data, point_layer_data)

  expect_equal(out, expected_out)
})


test_that("bind_layer_data works", {
  ggswim_obj <- ggswim(patient_data,
                       aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(data = end_study_events,
               aes(x = delta_t0, y = pt_id, color = end_study_name, label = end_study_label))

  label_layer_indices <- 2
  label_layer_data <- data.frame()

  out <- bind_layer_data(ggswim_obj, label_layer_indices, label_layer_data)

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "label", "size", "alpha", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)

  # Check for labels
  expected_labels <- c("✅", "❌", "⚠️")

  expect_true(all(expected_labels %in% out$label))
})
