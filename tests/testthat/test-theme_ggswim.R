test_that("theme_ggswim works", {
  theme_ggswim() |>
    class() |>
    expect_setequal(c("theme", "gg"))
})

test_that("theme_ggswim_dark works", {
  theme_ggswim_dark() |>
    class() |>
    expect_setequal(c("theme", "gg"))
})
