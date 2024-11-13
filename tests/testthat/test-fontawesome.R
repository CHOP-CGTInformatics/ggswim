test_that(".load_fonts works", {
  expect_no_condition(.load_fonts(verbose = FALSE))
})

test_that("search_fontawesome works", {
  all_vals <- search_fontawesome()
  specific_vals <- search_fontawesome(str = "fa-car")

  expect_equal(length(all_vals), nrow(FontAwesome))
  expect_true(all(stringr::str_detect(specific_vals, "car")))
})

test_that("fontawesome works", {
  out <- fontawesome("fa-dog")
  expect_equal(class(out), "character")
  expect_true(length(out) == 1)

  expect_message(fontawesome("notarealicon"), "Invalid: notarealicon")
  expect_true(is.na(fontawesome("notarealicon")) |> suppressMessages())
})
