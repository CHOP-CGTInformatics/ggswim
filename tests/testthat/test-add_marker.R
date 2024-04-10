infusions <- infusion_events |>
  mutate(infusion = dplyr::if_else(
    time_from_initial_infusion == 0, "Infusion", "Reinfusion"
  ))

initial_infusions <- infusions |>
  dplyr::filter(infusion == "Infusion")

reinfusions <- infusions |>
  dplyr::filter(infusion == "Reinfusion")

ggswim_layer <- ggswim(
  data = patient_data,
  aes(x = start_time, xend = end_time, y = pt_id, color = disease_assessment)
)

test_that("add_marker works for aes mapping", {
  mk1_layer <- add_marker(
    data = infusions,
    mapping = aes(x = time_from_initial_infusion, y = pt_id, color = infusion)
  )

  expect_setequal(class(mk1_layer), c("LayerInstance", "Layer", "ggproto", "gg"))
  expect_true("swim_class" %in% names(attributes(mk1_layer)))
  expect_true(attributes(mk1_layer)$swim_class == "marker_point")

  # Simple layer with aesthetic mapping ----
  p1 <- ggswim_layer +
    mk1_layer

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Simple marker addition on pt data",
    fig = p1
  )

  # Emoji layer aesthetic mapping ----
  mk2_layer <- add_marker(
    data = end_study_events,
    aes(x = time_from_initial_infusion, y = pt_id, label_vals = end_study_label, label_names = end_study_name)
  )

  p2 <- ggswim_layer +
    mk1_layer +
    mk2_layer

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Addition of labels layer, test for expected legend",
    fig = p2
  )
})

test_that("add_marker works for static aesthetics", {
  mk1_layer <- add_marker(
    data = initial_infusions,
    mapping = aes(x = time_from_initial_infusion, y = pt_id, name = "Infusion"),
    color = "red", size = 10
  ) |>
    suppressWarnings()

  p1 <- ggswim_layer +
    mk1_layer

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Single marker with static color callout",
    fig = p1
  )
})

test_that("error on fill argument", {
  expect_error(
    add_marker(
      data = patient_data,
      aes(x = start_time, xend = end_time, y = pt_id, fill = disease_assessment)
    ),
    class = "unsupported_aes"
  )
})

test_that("warn on label without color argument", {
  expect_warning(
    add_marker(
      data = end_study_events,
      aes(x = time_from_initial_infusion, y = pt_id, label_vals = end_study_label)
    ),
    class = "marker_label_aes"
  )
})
