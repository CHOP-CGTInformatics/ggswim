test_that("streamline works", {
  df <- tibble::tribble(
    ~subject_id, ~years, ~indicator_1, ~indicator_2, ~status,
    1, 2000, 1, 1, "unknown",
    1, 2001, 0, 0, "unknown",
    1, 2002, 1, 0, "positive",
    1, 2003, 0, 1, "negative",
    2, 2000, 0, 0, "positive",
    2, 2001, 0, 0, "negative",
    2, 2002, 0, 1, "negative",
    3, 2000, 0, 1, "negative",
    4, 2000, 0, 0, "negative",
    4, 2001, 1, 0, "positive",
    4, 2002, 0, 1, "positive"
  )

  out <- df |>
    streamline(subject_var = subject_id,
               time_var = years,
               markers = c(indicator_1, indicator_2),
               class_status = status)

  expected_list_elements <- c(
    "data",
    "markers",
    "subject_var",
    "time_var",
    "class_status"
  )

  expect_true(all(c("swim_tbl", "list") %in% class(out)))
  expect_true(all(expected_list_elements %in% names(out)))

  expect_error(streamline(df = 123), class = "check_data_frame")
})

test_that("add_columns works", {
  out <- mtcars |>
    add_columns(c("colA", "colB"))

  expect_true(all(c("colA", "colB") %in% names(out)))
  expect_true(ncol(out) > 0)
  expect_true(nrow(out) > 0)
  expect_equal(class(out), "data.frame")
})
