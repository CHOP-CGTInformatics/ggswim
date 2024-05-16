test_that("build_ggswim() returns the expected class and object", {
  # Use simple plot from helper.R
  p <- simple_plot()

  expect_no_error(build_ggswim(p))

  p_built <- build_ggswim(p)

  expect_true("ggswim_obj" %in% class(p))
  expect_false("ggswim_obj" %in% class(p_built))
})
