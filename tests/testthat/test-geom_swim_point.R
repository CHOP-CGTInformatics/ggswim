test_that("geom_swim_point is the right class", {
  layer <- geom_swim_point(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      color = infusion_type
    )
  )

  expect_equal(attr(layer, "class"), "marker_point")
})

test_that("all expected attributes exist in geom_swim_point", {
  layer <- geom_swim_point(
    data = infusion_events,
    mapping = aes(
      x = time_from_initial_infusion, y = pt_id,
      color = infusion_type
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
