pt_data <- tibble::tribble(
  ~"id", ~"trt", ~"end_time", ~"time",
  1, "Drug A", 5, 0,
  1, "Drug A", 5, 5,
  2, "Drug B", 2, 0,
  2, "Drug B", 2, 2,
  3, "Drug A", 4, 0,
  3, "Drug A", 4, 4,
  4, "Drug B", 7, 0,
  4, "Drug B", 7, 7
)

test_that("ggswim works for simple dataset", {

  p <- ggswim(pt_data, aes(x = time, y = id, fill = trt))

  vdiffr::expect_doppelganger(title = "Simple geom_col appears from ggswim",
                      fig = p)
})

test_that("error on color/colour argument", {
  expect_error(
    ggswim(pt_data, aes(x = time, y = id, color = trt)),
    class = "unsupported_aes"
  )
  expect_error(
    ggswim(pt_data, aes(x = time, y = id, colour = trt)),
    class = "unsupported_aes"
  )
})

test_that("test for expected attributes", {
  p <- ggswim(pt_data, aes(x = time, y = id, fill = trt))

  expect_setequal(class(p), c("ggswim_obj", "gg", "ggplot"))
  expect_true("swim_class" %in% names(attributes(p$layers[[1]])))
  expect_true(attributes(p$layers[[1]])$swim_class == "ggswim")
})
