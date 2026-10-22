arrow_plot <- function(data, scale = scale_arrow_discrete(), ...) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      xend = .data$end_time,
      y = .data$id,
      arrow = .data$event
    )
  ) +
    geom_swim_arrow(arrow_neck_length = 2, show.legend = TRUE, ...) +
    scale
}

test_that("arrow scale supplies its default style for one category", {
  data <- data.frame(id = "A", end_time = 10, event = "Continuation")

  built <- ggplot2::ggplot_build(arrow_plot(data))
  arrow <- built$data[[1]]$arrow

  expect_s3_class(arrow, "swim_arrow")
  expect_equal(vctrs::field(arrow, "colour"), "black")
  expect_equal(vctrs::field(arrow, "fill"), "black")
  expect_equal(vctrs::field(arrow, "type"), "closed")
})

test_that("arrow scale errors when categories request more styles than configured", {
  scale <- scale_arrow_discrete(
    colours = c("black", "red"),
    fills = c("gray20", "pink"),
    types = c("closed", "closed"),
    limits = c("A", "B")
  )

  expect_error(scale$palette(3), "3 categories requested, but only 2 styles")
})

test_that("arrow scale maps multiple categories when they share one head type", {
  data <- data.frame(
    id = c("A", "B"),
    end_time = c(10, 20),
    event = c("Continuation", "Ongoing")
  )
  scale <- scale_arrow_discrete(
    colours = c("black", "red"),
    fills = c("gray20", "pink"),
    types = c("closed", "closed"),
    limits = c("Continuation", "Ongoing")
  )

  built <- ggplot2::ggplot_build(arrow_plot(data, scale))
  arrow <- built$data[[1]]$arrow

  expect_equal(vctrs::field(arrow, "colour"), c("black", "red"))
  expect_equal(vctrs::field(arrow, "fill"), c("gray20", "pink"))
  expect_equal(vctrs::field(arrow, "type"), c("closed", "closed"))
})

test_that("arrow scales honor category limits and custom legend labels", {
  data <- data.frame(
    id = c("A", "B"),
    end_time = c(10, 20),
    event = c("A", "B")
  )
  scale <- scale_arrow_discrete(
    colours = c("red", "blue"),
    fills = c("pink", "lightblue"),
    types = c("closed", "closed"),
    limits = c("B", "A"),
    labels = c(A = "Alpha", B = "Beta")
  )

  built <- ggplot2::ggplot_build(arrow_plot(data, scale))
  arrow <- built$data[[1]]$arrow
  trained <- built$plot$scales$get_scales("arrow")

  expect_equal(vctrs::field(arrow, "colour"), c("blue", "red"))
  expect_equal(as.character(trained$get_labels()), c("Beta", "Alpha"))
})

test_that("missing and out-of-limit arrow values are not drawn", {
  data <- data.frame(
    id = letters[1:3],
    end_time = 10:12,
    event = c("Continuation", NA, "outside")
  )
  scale <- scale_arrow_discrete(
    colours = "black",
    fills = "black",
    types = "closed",
    limits = "Continuation"
  )

  built <- ggplot2::ggplot_build(arrow_plot(data, scale))

  expect_equal(
    vctrs::vec_detect_missing(built$data[[1]]$arrow),
    c(FALSE, TRUE, TRUE)
  )
})

test_that("faceted arrow panels receive the styles for their mapped categories", {
  data <- data.frame(
    id = c("A", "B"),
    end_time = c(10, 20),
    event = c("Continuation", "Ongoing"),
    panel = c("first", "second")
  )
  scale <- scale_arrow_discrete(
    colours = c("black", "red"),
    fills = c("gray20", "pink"),
    types = c("closed", "closed"),
    limits = c("Continuation", "Ongoing")
  )

  built <- ggplot2::ggplot_build(
    arrow_plot(data, scale) + ggplot2::facet_wrap(~panel)
  )
  layer <- built$data[[1]]
  first_panel <- layer[layer$PANEL == 1, , drop = FALSE]
  second_panel <- layer[layer$PANEL == 2, , drop = FALSE]

  expect_equal(vctrs::field(first_panel$arrow, "colour"), "black")
  expect_equal(vctrs::field(second_panel$arrow, "colour"), "red")
  expect_equal(vctrs::field(first_panel$arrow, "fill"), "gray20")
  expect_equal(vctrs::field(second_panel$arrow, "fill"), "pink")
})

test_that("scale_arrow_discrete does not translate missing values", {
  expect_false(scale_arrow_discrete()$na.translate)
})
