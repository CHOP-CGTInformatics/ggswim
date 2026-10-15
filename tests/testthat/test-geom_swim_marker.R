test_that("fixed marker strings give a mapping and scale hint", {
  withr::local_pdf(NULL)
  plot <- ggplot2::ggplot(data.frame(time = 1, id = "A"), aes(time, id)) +
    geom_swim_marker(marker = "+")

  expect_error(
    ggplot2::ggplotGrob(plot),
    "requires marker styles from a discrete scale"
  )
})

test_that("custom marker record classes with the required fields still draw", {
  withr::local_pdf(NULL)
  marker <- vctrs::new_rcrd(
    list(colour = "red", glyphs = "+"),
    class = "custom_marker"
  )
  scale <- ggplot2::discrete_scale(
    aesthetics = "marker",
    name = NULL,
    palette = function(n) marker[seq_len(n)],
    limits = "A",
    na.translate = FALSE
  )
  plot <- ggplot2::ggplot(
    data.frame(time = 1, id = "A", category = "A"),
    aes(time, id, marker = category)
  ) + geom_swim_marker() + scale

  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("geom_swim_marker works when inheriting data and associated params", {
  p <- ggplot(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion,
      y = pt_id,
      marker = label
    )
  ) +
    geom_swim_marker()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_marker inherited data and params",
    fig = p
  )
})

test_that("geom_swim_lane works when inheriting data and associated params", {
  p <- ggplot(
    data = patient_data,
    mapping = aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      colour = disease_assessment
    )
  ) +
    geom_swim_lane()

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_lane inherited data and params",
    fig = p
  )
})
