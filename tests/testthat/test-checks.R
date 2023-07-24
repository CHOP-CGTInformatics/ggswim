test_that("checkmate wrappers work", {
  # character
  expect_error(check_arg_is_character(123), class = "check_character")
  expect_true(check_arg_is_character("abc"))

  # choices
  expect_error(check_arg_is_dataframe(123), class = "check_data_frame")
  expect_true(check_arg_is_dataframe(mtcars))

  # integerish
  expect_error(check_arg_is_integerish("character"), class = "check_integerish")
  expect_true(check_arg_is_integerish(123))

  # list
  expect_error(check_arg_is_list(c("1", "2", "3")), class = "check_list")
  expect_true(check_arg_is_list(list("1", "2", "3")))

  # logical
  expect_error(check_arg_is_logical(123), class = "check_logical")
  expect_true(check_arg_is_logical(TRUE))
})
