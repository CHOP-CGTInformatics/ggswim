test_that("wrap_checkmate works", {
  out <- wrap_checkmate(checkmate::check_character)
  expect_equal(class(out), "function")
  expect_error(out(3), class = "ggswim_cond")
})

test_that("forma_error_val works", {
  out_atomic <- format_error_val("character")
  out_non_atomic <- format_error_val(data.frame())

  expect_equal(class(out_atomic), "character")
  expect_equal(class(out_non_atomic), "character")
})

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

test_that("check_supported_mapping_aes works", {
  mapping <- data.frame(
    "x" = "xvals",
    "y" = "yvals",
    "fill" = "fillvals",
    "colour" = "colourvals"
  )
  unsupported_aes <- c("fill")

  parent_func <- "test_function()"

  expect_error(
    check_supported_mapping_aes(mapping, unsupported_aes, parent_func),
    class = "unsupported_aes"
  )

  expect_no_error(
    check_supported_mapping_aes(mapping, unsupported_aes = NULL, parent_func)
  )
})

test_that("check_marker_label_aes works", {
  mapping_pass <- data.frame(
    "x" = "xvals",
    "y" = "yvals",
    "label" = "labelvals",
    "colour" = "colourvals"
  )

  mapping_warn <- data.frame(
    "x" = "xvals",
    "y" = "yvals",
    "label" = "labelvals"
  )
  expect_no_condition(check_marker_label_aes(mapping = mapping_pass))
  expect_warning(check_marker_label_aes(mapping = mapping_warn), class = "marker_label_aes")
})

test_that("check_arrow_fill_type works", {
  arrow_type_open <- "open"
  arrow_type_closed <- "closed"
  arrow_fill <- "red"

  expect_no_condition(
    check_arrow_fill_type(arrow_fill = arrow_fill, arrow_type = arrow_type_closed)
  )
  expect_warning(
    check_arrow_fill_type(arrow_fill = arrow_fill, arrow_type = arrow_type_open),
    class = "arrow_fill_type"
  )
})
