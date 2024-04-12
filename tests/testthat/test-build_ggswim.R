initial_infusions <- infusion_events |>
  dplyr::filter(time_from_initial_infusion == 0)

reinfusions <- infusion_events |>
  dplyr::filter(time_from_initial_infusion > 0)

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
  ggswim_obj <- ggswim(
    data = patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  ) +
    add_marker(
      data = end_study_events,
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        label_vals = end_study_label,
        label_names = end_study_name
      )
    )

  layer_indices <- 2L

  out <- suppressWarnings({
    bind_layer_data(ggswim_obj, layer_indices)
  })

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "label", "size", "alpha", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)

  # Check for labels
  expected_labels <- c("✅", "❌", "⚠️")

  expect_true(all(expected_labels %in% out$label))
})

test_that("bind_layer_data works for multiple layers", {
  initial_infusions <- infusion_events |>
    mutate(infusion = dplyr::if_else(
      time_from_initial_infusion == 0, "Infusion", "Reinfusion"
    )) |>
    dplyr::filter(infusion == "Infusion")

  reinfusions <- infusion_events |>
    mutate(infusion = dplyr::if_else(
      time_from_initial_infusion == 0, "Infusion", "Reinfusion"
    )) |>
    dplyr::filter(infusion == "Reinfusion")

  ggswim_obj <- ggswim(
    data = patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  ) +
    add_marker(initial_infusions,
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        color = infusion
      )
    ) +
    add_marker(reinfusions,
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        color = infusion
      )
    )

  layer_indices <- c(2, 3)

  out <- bind_layer_data(ggswim_obj, layer_indices)

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "shape", "size", "alpha", "stroke", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)
})

test_that("bind_layer_data works with fixed colors", {
  initial_infusions <- infusion_events |>
    mutate(infusion = dplyr::if_else(
      time_from_initial_infusion == 0, "Infusion", "Reinfusion"
    )) |>
    dplyr::filter(infusion == "Infusion")

  ggswim_obj <- suppressWarnings({
    ggswim_obj <- ggswim(
      data = patient_data,
      aes(
        x = start_time,
        xend = end_time,
        y = pt_id,
        color = disease_assessment
      )
    ) +
      add_marker(initial_infusions,
        mapping = aes(
          x = time_from_initial_infusion,
          y = pt_id,
          name = "Initial Infusion"
        ),
        color = "red"
      )
  })

  layer_indices <- 2L
  fixed_colours <- tibble::tribble(
    ~"indices", ~"colors", ~"name",
    2, "red", "Initial Infusion"
  )


  out <- bind_layer_data(ggswim_obj, layer_indices, fixed_colours)

  # Check for important columns
  expected_cols <- c("colour", "x", "y", "group", "shape", "size", "alpha", "stroke", "colour_mapping")

  expect_true(all(expected_cols %in% names(out)))
  expect_true(nrow(out) > 0)

  expect_true(all(out$colour == "red"))
})

test_that("get_ref_layer_info works for fixed colors", {
  initial_infusions <- infusion_events |>
    mutate(infusion = dplyr::if_else(
      time_from_initial_infusion == 0, "Infusion", "Reinfusion"
    )) |>
    dplyr::filter(infusion == "Infusion")

  ggswim_obj <- suppressWarnings({
    ggswim_obj <- ggswim(
      data = patient_data,
      aes(
        x = start_time,
        xend = end_time,
        y = pt_id,
        color = disease_assessment
      )
    ) +
      add_marker(initial_infusions,
        mapping = aes(
          x = time_from_initial_infusion,
          y = pt_id,
          name = "Initial Infusion"
        ),
        color = "red"
      )
  })

  expected_fixed <- list(
    label_layer_indices = NULL,
    point_layer_indices = 2,
    fixed_colours = data.frame(
      indices = 2,
      colors = "red",
      name = "Initial Infusion"
    )
  )

  out_fixed <- get_ref_layer_info(ggswim_obj)

  expect_equal(out_fixed, expected_fixed)
})


test_that("get_ref_layer_info works for point and label layers", {
  ggswim_obj <- ggswim(
    data = patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  ) +
    add_marker(infusion_events |> mutate(infusion = "Infusion"),
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        color = infusion
      )
    ) +
    add_marker(
      data = end_study_events,
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        label_vals = end_study_label,
        label_names = end_study_name
      )
    )

  expected <- list(
    label_layer_indices = 3,
    point_layer_indices = 2,
    fixed_colours = data.frame()
  )

  out <- get_ref_layer_info(ggswim_obj)

  expect_equal(out, expected)
})

test_that("ggswim_obj is appropriate class type", {
  ggswim_obj <- ggswim(
    data = patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  )

  ggswim_obj_markers <- ggswim(
    data = patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  ) +
    add_marker(infusion_events |> mutate(infusion = "Infusion"),
      mapping = aes(
        x = time_from_initial_infusion,
        y = pt_id,
        color = infusion
      )
    )

  expected <- c("gg", "ggplot", "ggswim_obj")

  expect_setequal(class(ggswim_obj), expected)
  expect_setequal(class(ggswim_obj_markers), expected)
})

test_that("build_ggswim errors on expected validation checks", {
  suppressWarnings({
    # Case 1: Multiple fixed_marker_names supplied
    ggswim_obj <- ggswim(
      data = patient_data,
      mapping = aes(
        x = start_time, xend = end_time, y = pt_id,
        color = disease_assessment
      ),
    ) +
      new_scale_color() +
      add_marker(
        data = initial_infusions,
        aes(x = time_from_initial_infusion, y = pt_id, name = "Initial Infusion"),
        color = "green", size = 5, fixed_marker_name = "Title 1"
      ) +
      add_marker(
        data = reinfusions,
        aes(x = time_from_initial_infusion + 2, y = pt_id, name = "Reinfusion"),
        color = "red", size = 5, fixed_marker_name = "Title 2"
      )
  })

  expect_error(build_ggswim(ggswim_obj),
    class = "multi_fixed_marker_scales"
  )

  suppressWarnings({
    # Case 2: Multiple new_scale_color()s supplied
    ggswim_obj <- ggswim(
      data = patient_data,
      mapping = aes(
        x = start_time, xend = end_time, y = pt_id,
        color = disease_assessment
      ),
    ) +
      new_scale_color() +
      add_marker(
        data = initial_infusions,
        aes(x = time_from_initial_infusion, y = pt_id, name = "Initial Infusion"),
        color = "green", size = 5, fixed_marker_name = "Title 1"
      ) +
      new_scale_color() +
      add_marker(
        data = reinfusions,
        aes(x = time_from_initial_infusion + 2, y = pt_id, name = "Reinfusion"),
        color = "red", size = 5, fixed_marker_name = "Title 1"
      )
  })

  expect_error(build_ggswim(ggswim_obj),
    class = "multi_fixed_marker_scales"
  )
})
