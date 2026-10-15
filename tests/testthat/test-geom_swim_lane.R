test_that("geom_swim_lane honors layer data with inherit.aes disabled", {
  parent <- data.frame(unused = 1)
  layer_data <- data.frame(start = c(1, 3), end = c(2, 5), id = c("A", "B"))
  plot <- ggplot2::ggplot(parent, aes(x = unused, y = unused)) +
    geom_swim_lane(
      data = layer_data,
      mapping = aes(x = start, xend = end, y = id),
      inherit.aes = FALSE
    )

  built <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(built$x, layer_data$start)
  expect_equal(built$xend, layer_data$end)
  expect_equal(built$y, c(1, 2))
})

test_that("geom_swim_lane reports and removes rows with missing endpoints", {
  withr::local_pdf(NULL)
  data <- data.frame(start = c(1, NA_real_), end = c(2, 3), id = 1:2)
  plot <- ggplot2::ggplot(data, aes(x = start, xend = end, y = id)) +
    geom_swim_lane()
  quiet_plot <- ggplot2::ggplot(data, aes(x = start, xend = end, y = id)) +
    geom_swim_lane(na.rm = TRUE)

  expect_warning(ggplot2::ggplotGrob(plot), "Removed 1 row")
  expect_no_warning(ggplot2::ggplotGrob(quiet_plot))
})

test_that("geom_swim_lane works when inheriting data and associated params", {
  p <- patient_data |>
    ggplot(
      mapping = aes(
        x = start_time,
        y = pt_id,
        xend = end_time,
        colour = disease_assessment
      )
    ) +
    geom_swim_lane()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane works with inherited data and params",
    fig = p
  )
})
