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
