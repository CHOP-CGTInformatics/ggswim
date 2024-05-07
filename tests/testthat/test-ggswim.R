test_that("ggswim works for simple dataset", {
  p <- ggswim(
    patient_data,
    aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment)
  )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Simple geom_segment() appears from ggswim",
    fig = p
  )
})

test_that("error on fill argument", {
  expect_error(
    ggswim(patient_data, aes(x = start_time, xend = end_time, y = pt_id, fill = disease_assessment)),
    class = "unsupported_aes"
  )
})

test_that("test for expected attributes", {
  p <- ggswim(patient_data, aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment))

  expect_setequal(class(p), c("ggswim_obj", "gg", "ggplot"))
  expect_true("swim_class" %in% names(p$layers[[1]]))
  expect_true(p$layers[[1]]$swim_class == "ggswim")
})

test_that("ggswim works with other layer types", {
  # This test looks for the inclusion of `geom_vline`, which makes for a new layer
  # We want to test that `build_ggswim()` doesn't fail on render and a vline appears

  p <- ggswim(
    patient_data,
    aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment)
  ) +
    ggplot2::geom_vline(xintercept = 0)

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "A vertical line appears on the ggswim plot",
    fig = p
  )
})

test_that("ggswim works for various and combined use cases", {
  initial_infusions <- infusion_events |>
    dplyr::filter(time_from_initial_infusion == 0)

  reinfusions <- infusion_events |>
    dplyr::filter(time_from_initial_infusion > 0)

  patient_status <- patient_data |>
    select(pt_id, end_time) |>
    unique()

  p_labels <- patient_data |>
    ggswim(mapping = aes(
      x = start_time, xend = end_time, y = pt_id,
      color = disease_assessment
    )) +
    new_scale_color() +
    add_marker(
      data = end_study_events,
      aes(x = time_from_initial_infusion, y = pt_id, label_vals = end_study_label, label_names = end_study_name)
    )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Case #1: ggswim with labels",
    fig = p_labels
  )

  suppressWarnings({
    p_fixed_points <- patient_data |>
      ggswim(mapping = aes(
        x = start_time, xend = end_time, y = pt_id,
        color = disease_assessment
      )) +
      add_marker(
        data = initial_infusions,
        aes(x = time_from_initial_infusion, y = pt_id, name = "Initial Infusion"),
        color = "green", shape = 22
      ) +
      add_marker(
        data = reinfusions,
        aes(x = time_from_initial_infusion + 2, y = pt_id, name = "Reinfusion"),
        color = "red", shape = 2
      )
  })

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Case #2: ggswim with fixed marker points",
    fig = p_fixed_points
  )

  p_dynamic_points <- patient_data |>
    ggswim(mapping = aes(
      x = start_time, xend = end_time, y = pt_id,
      color = disease_assessment
    )) +
    add_marker(
      data = infusion_events |> mutate(infusion = "Infusion"),
      aes(x = time_from_initial_infusion, y = pt_id, color = infusion)
    )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Case #3: ggswim with dynamic marker points",
    fig = p_dynamic_points
  )
})
