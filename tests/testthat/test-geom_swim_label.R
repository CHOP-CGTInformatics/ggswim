test_that("geom_swim_label is the right class", {
  layer <- geom_swim_label(
    data = end_study_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      label_vals = end_study_label,
      label_names = end_study_name
    )
  )

  expected_classes <- c(
    "swim_label", "LayerInstance", "Layer", "ggproto", "gg"
  )


  expect_setequal(attr(layer, "class"), expected_classes)
})

test_that("all expected attributes exist in geom_swim_label", {
  layer <- geom_swim_label(
    data = end_study_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      label_vals = end_study_label,
      label_names = end_study_name
    )
  )

  attrs <- names(attributes(layer))

  expected_attrs <- c(
    "class",
    "swim_class"
  )

  expect_true(all(expected_attrs %in% attrs))
})

test_that("geom_swim_label works when data is assigned from previous layer", {
  p <- ggplot2::ggplot(data = end_study_events) +
    geom_swim_label(
      mapping = aes(
        x = time_from_initial_infusion, y = pt_id,
        label_vals = end_study_label,
        label_names = end_study_name
      )
    )

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_label works with inherited data",
    fig = p
  )
})

test_that("geom_swim_label works when inheriting data and associated params", {
  p <- end_study_events |>
    ggplot(mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      label_vals = end_study_label, label_names = end_study_name
    )) +
    geom_swim_label(size = 5)

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "geom_swim_label works with inherited data and params",
    fig = p
  )
})
