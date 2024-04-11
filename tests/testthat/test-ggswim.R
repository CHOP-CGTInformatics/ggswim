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
  expect_true("swim_class" %in% names(attributes(p$layers[[1]])))
  expect_true(attributes(p$layers[[1]])$swim_class == "ggswim")
})

test_that("add_arrows works", {
  p_arrow <- add_arrows(
    data = patient_data,
    position = "identity",
    mapping = aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment),
    arrow = "status",
    # replicate defaults inherited from ggswim()
    arrow_type = "closed",
    arrow_colour = "black",
    arrow_fill = NULL,
    arrow_head_length = unit(0.25, "inches"),
    arrow_neck_length = NULL
  )

  expect_setequal(class(p_arrow), c("ggswim_obj", "gg", "ggproto", "LayerInstance", "Layer"))
  expect_true("swim_class" %in% names(attributes(p_arrow)))
  expect_true(attributes(p_arrow)$swim_class == "ggswim_arrows")

  skip_on_ci()
  p <- ggswim(
    patient_data,
    aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment)
  ) +
    p_arrow

  vdiffr::expect_doppelganger(
    title = "Arrows appear with add_arrows() external",
    fig = p
  )

  p <- ggswim(patient_data,
    aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment),
    arrow = status,
    arrow_neck_length = status_length
  )

  vdiffr::expect_doppelganger(
    title = "Arrows appear with internal ggswim() method",
    fig = p
  )


  # Check for logical supplied to arg `arrow`
  expect_error(
    add_arrows(
      data = patient_data,
      mapping = aes(x = start_time, y = id),
      position = "identity",
      arrow = end_time,
      # replicate defaults inherited from ggswim()
      arrow_type = "closed",
      arrow_colour = "black",
      arrow_fill = NULL,
      arrow_head_length = unit(0.25, "inches"),
      arrow_neck_length = NULL
    ),
    class = "ggswim_cond"
  )
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
    select(pt_id, end_time, status, status_length) |>
    unique() |>
    dplyr::rename("arrow" = status, "time_from_today" = status_length)

  p_external_arrows <- patient_data |>
    ggswim(mapping = aes(
      x = start_time, xend = end_time, y = pt_id,
      color = disease_assessment
    ), linewidth = 15) +
    add_arrows(
      data = patient_status,
      mapping = aes(xend = end_time, y = pt_id),
      arrow = arrow,
      arrow_neck_length = time_from_today,
      arrow_colour = "forestgreen", arrow_fill = "forestgreen"
    )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Case 1: ggswim works with add_arrows()",
    fig = p_external_arrows
  )

  p_internal_arrows <- patient_data |>
    ggswim(
      mapping = aes(
        x = start_time, xend = end_time, y = pt_id,
        color = disease_assessment
      ),
      arrow = status,
      arrow_neck_length = status_length,
      arrow_colour = "forestgreen", arrow_fill = "cyan"
    )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Case #2: ggswim with internal arrows",
    fig = p_internal_arrows
  )

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
    title = "Case #3: ggswim with labels",
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
    title = "Case #4: ggswim with fixed marker points",
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
    title = "Case #5: ggswim with dynamic marker points",
    fig = p_dynamic_points
  )
})
