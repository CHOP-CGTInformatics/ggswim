test_that("add_arrows works", {
  no_arrow_statuses <- c(
    "Completed Study Follow-Up",
    "Deceased",
    "Other End Study Reason"
  )

  arrow_data <- patient_data |>
    dplyr::left_join(
      end_study_events |>
        dplyr::select(pt_id, end_study_name),
      by = "pt_id"
    ) |>
    dplyr::select(pt_id, end_time, end_study_name) |>
    dplyr::filter(.by = pt_id, end_time == max(end_time))


  p_arrow <- add_arrows(
    data = arrow_data,
    position = "identity",
    mapping = aes(xend = end_time, y = pt_id),
    arrow_type = "closed",
    arrow_colour = "black",
    arrow_fill = NULL,
    arrow_head_length = unit(0.25, "inches"),
    arrow_neck_length = NULL
  )

  expect_setequal(class(p_arrow), c("ggswim_obj", "gg", "ggproto", "LayerInstance", "Layer"))
  expect_true("swim_class" %in% names(p_arrow))
  expect_true(p_arrow$swim_class == "ggswim_arrows")

  skip_on_ci()
  p <- simple_plot() +
    p_arrow

  vdiffr::expect_doppelganger(
    title = "Arrows appear with add_arrows()",
    fig = p
  )
})
