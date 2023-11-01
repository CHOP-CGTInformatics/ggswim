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

test_that("ggswim works for simple dataset", {
  p <- ggswim(pt_data, aes(x = time, y = id, fill = trt))

  vdiffr::expect_doppelganger(
    title = "Simple geom_col appears from ggswim",
    fig = p
  )
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

test_that("add_arrows works", {
  p <- ggswim(pt_data, aes(x = time, y = id))

  p_arrow <- add_arrows(
    data = pt_data,
    ggswim_obj = p,
    mapping = aes(x = time, y = id),
    arrow = "alive",
    # replicate defaults inherited from ggswim()
    arrow_type = "closed",
    arrow_colour = "black",
    arrow_fill = NULL,
    arrow_length = unit(0.25, "inches")
  )

  expect_setequal(class(p_arrow), c("ggswim_obj", "gg", "ggplot"))
  expect_true("swim_class" %in% names(attributes(p_arrow$layers[[1]])))
  expect_true("swim_class" %in% names(attributes(p_arrow$layers[[2]])))
  expect_true(attributes(p_arrow$layers[[1]])$swim_class == "ggswim")
  expect_true(attributes(p_arrow$layers[[2]])$swim_class == "ggswim")

  vdiffr::expect_doppelganger(
    title = "Arrows appear using default values",
    fig = p_arrow
  )

  # Check for logical supplied to arg `arrow`
  expect_error(
    add_arrows(
      data = pt_data,
      ggswim_obj = p,
      mapping = aes(x = time, y = id),
      arrow = "cohort",
      # replicate defaults inherited from ggswim()
      arrow_type = "closed",
      arrow_colour = "black",
      arrow_fill = NULL,
      arrow_length = unit(0.25, "inches")
    ),
    class = "ggswim_cond"
  )
})

test_that("ggswim throws a warning when there is missing data", {
  missing_data <- tibble::tribble(
    ~"id", ~"val1", ~"val2",
    1, "A", 1,
    2, NA, 2
  )

  p_missing <- ggswim(pt_data, aes(x = time, y = id, fill = trt)) +
    add_marker(data = missing_data,
               mapping = aes(x = val2, y = id, colour = val1))

  expect_warning(p_missing |> build_ggswim(), class = "missing_colour_data")

  suppressWarnings(
    vdiffr::expect_doppelganger(
      title = "Missing data appears in ggswim plot, but is dropped from legend",
      fig = p_missing
    )
  )
})

test_that("ggswim works with arrow arguments", {
  p <- ggswim(pt_data, aes(x = time, y = id, fill = trt),
              arrow = alive)

  vdiffr::expect_doppelganger(
    title = "Arrows appear in ggswim with defaults",
    fig = p
  )
})

test_that("ggswim works with other layer types", {
  # This test looks for the inclusion of `geom_vline`, which makes for a new layer
  # We want to test that `build_ggswim()` doesn't fail on render and a vline appears

  pt_data_neg <- tibble::tribble(
    ~"id", ~"trt", ~"end_time", ~"time", ~"alive",
    1, "Drug A", 5, -5, TRUE,
    1, "Drug A", 5, 5, TRUE,
    2, "Drug B", 2, -10, FALSE,
    2, "Drug B", 2, 2, FALSE,
    3, "Drug A", 4, 0, FALSE,
    3, "Drug A", 4, 4, FALSE,
    4, "Drug B", 7, 3, TRUE,
    4, "Drug B", 7, 7, TRUE
  )

  p <- ggswim(data = pt_data_neg, aes(x = time, y = id, fill = trt)) +
    ggplot2::geom_vline(xintercept = 0)

  vdiffr::expect_doppelganger(
    title = "A vertical line appears on the ggswim plot",
    fig = p
  )
})
