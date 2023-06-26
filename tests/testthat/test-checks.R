test_that("checkmate wrappers work", {
  # swim_tbl
  expect_error(check_arg_is_swim_tbl(123), class = "check_swim_tbl")

  # character
  expect_error(check_arg_is_character(123), class = "check_character")
  expect_true(check_arg_is_character("abc"))

  # logical
  expect_error(check_arg_is_logical(123), class = "check_logical")
  expect_true(check_arg_is_logical(TRUE))

  # choices
  expect_error(check_arg_is_dataframe(123), class = "check_data_frame")
  expect_true(check_arg_is_dataframe(mtcars))
})
