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
    dplyr::select(pt_id, start_time, end_time, end_study_name) |>
    dplyr::filter(.by = pt_id, end_time == max(end_time)) |>
    unique() |>
    dplyr::mutate(status = ifelse(!end_study_name %in% no_arrow_statuses, TRUE, FALSE))


  p_arrow <- add_arrows(
    data = arrow_data,
    position = "identity",
    mapping = aes(x = start_time, xend = end_time, y = pt_id),
    arrow = "status",
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
  p <- ggswim(
    patient_data,
    aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment)
  ) +
    p_arrow

  vdiffr::expect_doppelganger(
    title = "Arrows appear with add_arrows()",
    fig = p
  )

  # Check for logical supplied to arg `arrow`
  expect_error(
    add_arrows(
      data = patient_data,
      mapping = aes(x = start_time, y = id),
      position = "identity",
      arrow = end_time,
      arrow_type = "closed",
      arrow_colour = "black",
      arrow_fill = NULL,
      arrow_head_length = unit(0.25, "inches"),
      arrow_neck_length = NULL
    ),
    class = "ggswim_cond"
  )
})
