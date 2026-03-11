test_that("theme_ggswim works", {
  out <- theme_ggswim()
  expect_true(all(c("theme", "gg") %in% class(out)))
})

test_that("theme_ggswim_dark works", {
  out <- theme_ggswim_dark()
  expect_true(all(c("theme", "gg") %in% class(out)))
})
