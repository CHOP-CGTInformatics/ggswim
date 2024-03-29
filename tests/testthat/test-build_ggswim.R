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


test_that("bind_layer_data works for single layer", {
  ggswim_obj <- ggswim(patient_data,
                       aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(data = end_study_events,
               aes(x = delta_t0, y = pt_id, color = end_study_name,
                   label_vals = end_study_label, label_names = end_study_name))

  layer_indices <- 2L
  layer_data <- data.frame()

  out <- bind_layer_data(ggswim_obj, layer_indices, layer_data) |>
    suppressWarnings() # Duplicates aesthetic warning

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "label", "size", "alpha", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)

  # Check for labels
  expected_labels <- c("✅", "❌", "⚠️")

  expect_true(all(expected_labels %in% out$label))
})

test_that("bind_layer_data works for multiple layers", {
  aplasia <- patient_data |>
    dplyr::filter(bcell_status == "B-cell Aplasia")

  recovery <- patient_data |>
    dplyr::filter(bcell_status == "B-cell Recovery")

  ggswim_obj <- ggswim(patient_data,
                       aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(aplasia,
               mapping = aes(x = delta_t0, y = pt_id, color = bcell_status)) +
    add_marker(recovery,
               mapping = aes(x = delta_t0, y = pt_id, color = bcell_status))

  layer_indices <- 3
  layer_data <- data.frame()

  out <- bind_layer_data(ggswim_obj, layer_indices, layer_data)

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "shape", "size", "alpha", "stroke", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)
})

test_that("bind_layer_data works with static colors", {
  aplasia <- patient_data |>
    dplyr::filter(bcell_status == "B-cell Aplasia")

  ggswim_obj <- ggswim(patient_data,
                       aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(aplasia,
               mapping = aes(x = delta_t0, y = pt_id, name = "B-cell Aplasia"), color = "red") |>
      suppressWarnings()

  layer_indices <- 2L
  layer_data <- data.frame()
  static_colours <- tibble::tribble(
    ~"indices", ~"colors", ~"name",
    2, "red", "B-cell Aplasia"
  )


  out <- bind_layer_data(ggswim_obj, layer_indices, layer_data, static_colours)

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "shape", "size", "alpha", "stroke", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)

  expect_true(all(out$colour == "red"))
})

test_that("get_ref_layer_info works for static colors", {
  aplasia <- patient_data |>
    dplyr::filter(bcell_status == "B-cell Aplasia")

  ggswim_obj <- ggswim(patient_data, aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(aplasia,
               mapping = aes(x = delta_t0, y = pt_id, name = "B-cell Aplasia"), color = "red") |>
      suppressWarnings()

  expected_static <- list(
    label_layer_indices = NULL,
    point_layer_indices = 2,
    static_colours = data.frame(
      indices = 2,
      colors = "red",
      name = "B-cell Aplasia"
    )
  )

  out_static <- get_ref_layer_info(ggswim_obj)

  expect_equal(out_static, expected_static)
})


test_that("get_ref_layer_info works for point and label layers", {

  ggswim_obj <- ggswim(patient_data, aes(x = delta_t0, y = pt_id, fill = disease_assessment_status)) +
    add_marker(infusion_events,
               mapping = aes(x = infusion_delta_t0, y = pt_id, color = infusion_type, shape = infusion_type)) +
    add_marker(end_study_events,
               mapping = aes(x = delta_t0, y = pt_id, label_vals = end_study_label, label_names = end_study_name)) |>
      suppressWarnings()

  expected <- list(
    label_layer_indices = 3,
    point_layer_indices = 2,
    static_colours = data.frame()
  )

  out <- get_ref_layer_info(ggswim_obj)

  expect_equal(out, expected)
})
