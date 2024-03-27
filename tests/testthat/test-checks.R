pt_data <- tibble::tribble(
  ~"id", ~"trt", ~"end_time", ~"time", ~"alive",
  1, "Drug A", 5, 0, TRUE,
  1, "Drug A", 5, 5, TRUE,
  2, "Drug B", 2, 0, FALSE,
  2, "Drug B", 2, 2, FALSE,
  3, "Drug A", 4, 0, FALSE,
  3, "Drug A", 4, 4, FALSE,
  4, "Drug B", 7, 0, TRUE,
  4, "Drug B", 7, 7, TRUE
)

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
    "label_vals" = "labelvals",
    "label_names" = "colourvals"
  )

  mapping_warn <- data.frame(
    "x" = "xvals",
    "y" = "yvals",
    "label_vals" = "labelvals"
  )

  mapping_error <- data.frame(
    "x" = "xvals",
    "y" = "yvals",
    "label_names" = "label_names"
  )

  expect_no_condition(check_marker_label_aes(mapping = mapping_pass))
  expect_warning(check_marker_label_aes(mapping = mapping_warn), class = "marker_label_aes")
  expect_error(check_marker_label_aes(mapping = mapping_error), class = "marker_label_aes")
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

test_that("check_arrow_neck_length works", {
  accepted_val_1 <- 15
  accepted_val_2 <- as.name("test")
  unnaccepted_val_1 <- TRUE
  unnaccepted_val_2 <- "test"

  expect_no_error(
    check_arrow_neck_length(arrow_neck_length = accepted_val_1)
  )

  expect_no_error(
    check_arrow_neck_length(arrow_neck_length = accepted_val_2)
  )

  expect_error(
    check_arrow_neck_length(arrow_neck_length = unnaccepted_val_1),
    class = "arrow_neck_length_class"
  )

  expect_error(
    check_arrow_neck_length(arrow_neck_length = unnaccepted_val_2),
    class = "arrow_neck_length_class"
  )
})

test_that("check_ggswim_obj works", {
  ggswim_obj <- ggswim(pt_data, aes(x = time, y = id, fill = trt))
  non_ggswim_obj <- ggplot(mtcars) +
    geom_point(aes(x = cyl, y = hp))

  # Using default enabled print method
  expect_no_error(
    check_ggswim_obj(ggswim_obj)
  )
  expect_error(
    check_ggswim_obj(non_ggswim_obj),
    class = "ggswim_obj_class"
  )
})

test_that("check_coerced_data works", {
  valid_vector <- c("colour")
  invalid_vector <- c("factor", "colour")

  expect_no_error(
    check_coerced_data(valid_vector)
  )
  expect_error(
    check_coerced_data(invalid_vector),
    class = "coerced_vars"
  )
})

test_that("check_supported_position_args works", {
  parent_func <- "test_function()"

  expect_error(
    check_supported_position_args(position = "test", parent_func),
    class = "unsupported_position"
  )

  expect_no_error(
    check_supported_position_args(position = "identity", parent_func)
  )
})
