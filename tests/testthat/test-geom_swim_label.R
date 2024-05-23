test_that("geom_swim_label is the right class", {
  layer <- geom_swim_label(
    data = end_study_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      label_vals = end_study_label,
      label_names = end_study_name
    )
  )

  expect_setequal(attr(layer, "class"), c("swim_label", "ggswim_layer"))
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
    "stat",
    "position",
    "mapping",
    "data",
    "show.legend",
    "inherit.aes",
    "params"
  )

  expect_true(all(expected_attrs %in% attrs))
})
