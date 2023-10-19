test_that("theme_ggswim works", {
  theme_ggswim() |>
    class() |>
    expect_setequal(c("theme", "gg"))
})
