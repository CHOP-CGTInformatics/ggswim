test_that("retrieve_original_aes works", {
  data <- mtcars
  coerced_mapping <- aes(x = "cyl", y = "hp", color = factor("disp"))
  non_coerced_mapping <- aes(x = "cyl", y = "hp", color = "disp")

  expect_no_error(
    retrieve_original_aes(data = data, aes_mapping = unlist(non_coerced_mapping), aes_var = "colour")
  )
  expect_no_error(
    retrieve_original_aes(data = data, aes_mapping = unlist(coerced_mapping), aes_var = "colour")
  )

  expect_equal(
    retrieve_original_aes(data = data, aes_mapping = unlist(non_coerced_mapping), aes_var = "colour"),
    retrieve_original_aes(data = data, aes_mapping = unlist(coerced_mapping), aes_var = "colour")
  )

  expect_true(is.character(retrieve_original_aes(
    data = data,
    aes_mapping = unlist(coerced_mapping),
    aes_var = "colour"
  )))
  expect_true(length(retrieve_original_aes(
    data = data,
    aes_mapping = unlist(coerced_mapping),
    aes_var = "colour"
  )) == 1)
})

test_that("try_ggswim captures expected errors", {
  tbl <- tibble::tribble(
    ~"record", ~"status", ~"start_time", ~"end_time", ~"marker1", ~"marker1_time",
    ~"marker2", ~"marker2_time", ~"marker2_name",
    1, "status1", 0, 5, "marker1", 3, "❌", 5, "Negative",
    2, "status1", -2, 7, "marker1", 4, "✅", 6, "Positive",
    3, "status2", 2, 15, "marker2", 10, "❌", 15, "Negative"
  )

  # Point before label ordering has no error
  p <- ggplot(data = tbl) +
    geom_swim_lane(
      mapping = aes(x = start_time, xend = end_time, y = record, colour = status)
    ) +
    geom_swim_point(mapping = aes(x = marker1_time, y = record, colour = marker1)) +
    geom_swim_label(mapping = aes(x = marker2_time, y = record, label_vals = marker2, label_names = marker2_name))

  expect_no_error(p)

  # Label before point ordering with new_scale_color has no error
  p <- ggplot(data = tbl) +
    geom_swim_lane(
      mapping = aes(x = start_time, xend = end_time, y = record, colour = status)
    ) +
    geom_swim_label(mapping = aes(x = marker2_time, y = record, label_vals = marker2, label_names = marker2_name)) +
    new_scale_color() +
    geom_swim_point(mapping = aes(x = marker1_time, y = record, colour = marker1))

  expect_no_error(p)


  # Error occurs without new_scale_colour and label before point ordering
  p <- ggplot(data = tbl) +
    geom_swim_lane(
      mapping = aes(x = start_time, xend = end_time, y = record, colour = status)
    ) +
    geom_swim_label(mapping = aes(x = marker2_time, y = record, label_vals = marker2, label_names = marker2_name)) +
    geom_swim_point(mapping = aes(x = marker1_time, y = record, colour = marker1))

  expect_error(print(p), class = "scale_replacement_error")
})
