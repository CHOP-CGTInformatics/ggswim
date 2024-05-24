test_that("grid.draw method works", {
  out <- grid.draw.ggswim_obj(no_label_plot())
  expect_setequal(class(out), c("gg", "ggplot"))
})
