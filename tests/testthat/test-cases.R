test_that("geom_swim_lane works with inherited data", {
  p <- patient_data |>
    ggplot2::ggplot(mapping = aes(x = start_time, y = pt_id, colour = disease_assessment)) +
    geom_swim_lane(mapping = aes(xend = end_time))

  expect_setequal(class(p), c("ggswim_obj", "gg", "ggplot"))
  expect_true(is_empty(p$layers[[1]]$data))

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane works with inherited data",
    fig = p
  )
})

test_that("geom_swim_point works with inherited data", {
  p <- infusion_events |>
    ggplot2::ggplot(mapping = aes(x = time_from_initial_infusion, y = pt_id, colour = infusion_type)) +
    geom_swim_point()

  expect_setequal(class(p), c("ggswim_obj", "gg", "ggplot"))
  expect_true(is_empty(p$layers[[1]]$data))

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_point works with inherited data",
    fig = p
  )
})

test_that("geom_swim_label works with inherited data", {
  p <- end_study_events |>
    ggplot2::ggplot(mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      label_vals = end_study_label, label_names = end_study_name
    )) +
    geom_swim_point()

  expect_setequal(class(p), c("ggswim_obj", "gg", "ggplot"))
  expect_true(is_empty(p$layers[[1]]$data))

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_label works with inherited data",
    fig = p
  )
})
