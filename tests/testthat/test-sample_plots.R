# Not to be run in CI, these tests examine plot output snapshots

test_that("single row data with 2 new scales", {
  single_row_data <- tibble::tribble(
    ~"record", ~"status", ~"start_time", ~"end_time", ~"marker1", ~"marker1_time",
    ~"marker2", ~"marker2_time", ~"marker2_name",
    1, "status1", 0, 5, "marker1", 3, "❌", 5, "Negative",
    2, "status1", -2, 7, "marker1", 4, "✅", 6, "Positive",
    3, "status2", 2, 15, "marker2", 10, "⭕", 15, "Assessing"
  )

  single_row_data$record <- factor(single_row_data$record)

  p <- ggplot2::ggplot() +
    geom_swim_lane(
      data = single_row_data,
      mapping = aes(x = start_time, xend = end_time, y = record, colour = status), linewidth = 2
    ) +
    scale_color_manual(name = "Lanes", values = c("red", "blue")) +
    new_scale_color() +
    geom_swim_point(
      data = single_row_data, mapping = aes(x = marker1_time, y = record, colour = marker1),
      size = 5
    ) +
    scale_color_manual(name = "Marker Points", values = c("green", "orange")) +
    new_scale_color() +
    geom_swim_label(
      data = single_row_data, mapping = aes(x = marker2_time, y = record,
                                            label_vals = marker2, label_names = marker2_name),
      label.size = NA, size = 5
    ) +
    scale_color_manual(name = "Marker Labels", values = c(NA, NA, NA)) +
    theme_minimal()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "single row data with 2 new scales",
    fig = p
  )
})

test_that("single row data with 1 new scale", {
  single_row_data <- tibble::tribble(
    ~"record", ~"status", ~"start_time", ~"end_time", ~"marker1", ~"marker1_time",
    ~"marker2", ~"marker2_time", ~"marker2_name",
    1, "status1", 0, 5, "marker1", 3, "❌", 5, "Negative",
    2, "status1", -2, 7, "marker1", 4, "✅", 6, "Positive",
    3, "status2", 2, 15, "marker2", 10, "⭕", 15, "Assessing"
  )

  single_row_data$record <- factor(single_row_data$record)

  p <- ggplot2::ggplot() +
    geom_swim_lane(
      data = single_row_data,
      mapping = aes(x = start_time, xend = end_time, y = record, colour = status), linewidth = 2
    ) +
    scale_color_manual(name = "Lanes", values = c("red", "blue")) +
    new_scale_color() + # Where we would expect to apply new_scale_color
    geom_swim_point(
      data = single_row_data, mapping = aes(x = marker1_time, y = record, colour = marker1),
      size = 5
    ) +
    geom_swim_label(
      data = single_row_data, mapping = aes(x = marker2_time, y = record,
                                            label_vals = marker2, label_names = marker2_name),
      label.size = NA, size = 5
    ) +
    scale_color_manual(name = "All Markers", values = c(NA, "green", "orange", NA, NA)) +
    theme_minimal()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "single row data with 1 new scale",
    fig = p
  )
})

test_that("patient data sets with 2 new scales", {
  p <- patient_data |>
    ggplot() +
    geom_swim_lane(mapping = aes(x = start_time, y = pt_id, xend = end_time, color = disease_assessment)) +
    ggplot2::scale_color_brewer(name = "Lanes", palette = "Set1") +
    new_scale_color() +
    geom_swim_point(
      data = infusion_events,
      mapping = aes(x = time_from_initial_infusion, y = pt_id, color = infusion_type),
      size = 5
    ) +
    ggplot2::scale_color_manual(name = "Markers", values = c("red", "green")) +
    new_scale_color() +
    geom_swim_label(
      data = end_study_events,
      mapping = aes(x = time_from_initial_infusion, y = pt_id,
                    label_vals = end_study_label, label_names = end_study_name), # nolint object_usage_linter
      label.size = NA, size = 5
    ) +
    ggplot2::scale_color_brewer(name = "Labels", palette = 1) +
    theme_ggswim()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "patient datasets with 2 new scales",
    fig = p
  )
})
