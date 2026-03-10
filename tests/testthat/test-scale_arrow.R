test_that("scale_arrow_discrete creates a discrete scale for the arrow aesthetic", {
  sc <- scale_arrow_discrete()

  expect_s3_class(sc, "ScaleDiscrete")
  expect_equal(sc$aesthetics, "arrow")
  expect_false(sc$na.translate)
})

test_that("scale_arrow_discrete palette returns swim_arrow records", {
  sc <- scale_arrow_discrete(
    colours = c("black", "red"),
    fills   = c("black", "pink"),
    types   = c("closed", "open"),
    limits  = c("Continuation", "Ongoing")
  )

  vals <- sc$palette(2)

  expect_s3_class(vals, "vctrs_rcrd")
  expect_equal(class(vals)[1], "swim_arrow")
  expect_equal(vctrs::field(vals, "colour"), c("black", "red"))
  expect_equal(vctrs::field(vals, "fill"), c("black", "pink"))
  expect_equal(vctrs::field(vals, "type"), c("closed", "open"))
})

test_that("scale_arrow_discrete respects limits", {
  sc <- scale_arrow_discrete(
    colours = c("black", "red"),
    fills   = c("black", "red"),
    types   = c("closed", "open"),
    limits  = c("Continuation", "Withdrawn")
  )

  expect_equal(sc$limits, c("Continuation", "Withdrawn"))
})

test_that("scale_arrow_discrete uses defaults when values are not supplied", {
  sc <- scale_arrow_discrete(limits = "Continuation")
  vals <- sc$palette(1)

  expect_equal(vctrs::field(vals, "colour"), .default_arrow_colours)
  expect_equal(vctrs::field(vals, "fill"), .default_arrow_fills)
  expect_equal(vctrs::field(vals, "type"), .default_arrow_types)
})

test_that("pal_arrows warns when more values are requested than available", {
  pal <- pal_arrows(
    colours = c("black", "red"),
    fills   = c("black", "red"),
    types   = c("closed", "open"),
    n_values = 2
  )

  expect_error(
    pal(3),
    "Can't subset elements past the end."
  )
})

test_that("scale_arrow_discrete works in a ggplot build", {
  df <- data.frame(
    id = factor(c("A", "B")),
    end_time = c(10, 20),
    label = c("Continuation", "Continuation")
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(
        xend = end_time,
        y = id,
        arrow = "Continuation"
      ),
      arrow_neck_length = 2,
      show.legend = c(arrow = TRUE)
    ) +
    scale_arrow_discrete(
      limits = "Continuation",
      colours = "black",
      fills = "black",
      types = "closed"
    )

  built <- ggplot2::ggplot_build(p)
  layer_data <- built$data[[1]]

  expect_true("arrow" %in% names(layer_data))
  expect_s3_class(layer_data$arrow, "vctrs_rcrd")
  expect_equal(unique(vctrs::field(layer_data$arrow, "colour")), "black")
  expect_equal(unique(vctrs::field(layer_data$arrow, "fill")), "black")
  expect_equal(unique(vctrs::field(layer_data$arrow, "type")), "closed")
})
