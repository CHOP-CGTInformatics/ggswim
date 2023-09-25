test_that("grid.draw method works", {
  out <- grid.draw.ggswim_obj(ggswim(mtcars, aes(x = cyl, y = hp)))
  expect_setequal(class(out), c("gg", "ggplot"))
})
