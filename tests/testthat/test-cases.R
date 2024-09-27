test_that("geom_swim_lane works with inherited data", {
  p <- patient_data |>
    ggplot2::ggplot(mapping = aes(x = start_time, y = pt_id, colour = disease_assessment)) +
    geom_swim_lane(mapping = aes(xend = end_time))

  expect_true(is_empty(p$layers[[1]]$data))

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane works with inherited data",
    fig = p
  )
})
