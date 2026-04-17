test_that("geom_swim_arrow creates a layer with GeomSwimArrow", {
  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = data.frame(y = 1, xend = 10),
      ggplot2::aes(y = y, xend = xend)
    )

  expect_length(p$layers, 1)
  expect_identical(p$layers[[1]]$geom, GeomSwimArrow)
})

test_that("geom_swim_arrow passes parameters through layer", {
  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = data.frame(y = 1, xend = 10),
      ggplot2::aes(y = y, xend = xend),
      arrow_colour = "red",
      arrow_fill = "blue",
      arrow_type = "open",
      arrow_neck_length = 5,
      linewidth = 0.7
    )

  params <- p$layers[[1]]$geom_params

  expect_equal(params$arrow_colour, "red")
  expect_equal(params$arrow.fill, "blue")
  expect_equal(params$arrow_type, "open")
  expect_equal(params$arrow_neck_length, 5)
})

test_that("GeomSwimArrow setup_data uses supplied arrow_neck_length", {
  data <- data.frame(y = c(1, 2), xend = c(10, 20))

  out <- GeomSwimArrow$setup_data(
    data,
    params = list(arrow_neck_length = 5)
  )

  expect_equal(out$x, c(10, 20))
  expect_equal(out$xend, c(15, 25))
})

test_that("GeomSwimArrow setup_data uses proportional default arrow_neck_length", {
  data <- data.frame(y = c(1, 2), xend = c(10, 20))

  out <- GeomSwimArrow$setup_data(
    data,
    params = list(arrow_neck_length = NULL)
  )

  expect_equal(out$x, c(10, 20))
  expect_equal(out$xend, c(13, 23))
})

test_that("GeomSwimArrow setup_data handles missing xend values", {
  data <- data.frame(y = c(1, 2, 3), xend = c(10, NA, 20))

  out <- GeomSwimArrow$setup_data(
    data,
    params = list(arrow_neck_length = NULL)
  )

  expect_equal(out$x, c(10, NA, 20))
  expect_equal(out$xend, c(13, NA, 23))
})

test_that("geom_swim_arrow works with arrow aesthetic after ggplot_build", {
  df <- data.frame(
    id = factor(c("A", "B")),
    end_time = c(10, 20),
    label = c("Continuation", "Continuation")
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(y = id, xend = end_time, arrow = "Continuation"),
      arrow_neck_length = 2
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

test_that("geom_swim_arrow stores fallback params when arrow aesthetic is absent", {
  df <- data.frame(
    id = factor(c("A", "B")),
    end_time = c(10, 20)
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(y = id, xend = end_time),
      arrow_colour = "red",
      arrow_fill = "red",
      arrow_type = "closed",
      arrow_neck_length = 2
    )

  params <- p$layers[[1]]$geom_params

  expect_equal(params$arrow_colour, "red")
  expect_equal(params$arrow.fill, "red")
  expect_equal(params$arrow_type, "closed")
  expect_equal(params$arrow_neck_length, 2)
})

test_that("geom_swim_arrow errors when multiple arrow types occur in one layer", {
  df <- data.frame(
    id = factor(c("A", "B")),
    end_time = c(10, 20),
    label = c("A", "B")
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(y = id, xend = end_time, arrow = label),
      arrow_neck_length = 2
    ) +
    scale_arrow_discrete(
      limits = c("A", "B"),
      colours = c("black", "black"),
      fills = c("black", "black"),
      types = c("closed", "open")
    )

  expect_error(
    print(p),
    "currently supports only one arrow type per layer"
  )
})

test_that("draw_key_swim_arrow returns a grob", {
  key_data <- data.frame(
    colour = "black",
    fill = "black",
    linewidth = 0.6,
    linetype = 1,
    alpha = NA_real_
  )

  key_data$arrow <- vctrs::new_rcrd(
    list(
      colour = "black",
      fill = "black",
      type = "closed"
    ),
    class = "swim_arrow"
  )

  grob <- draw_key_swim_arrow(
    data = key_data,
    params = list(arrow_head_length = grid::unit(0.15, "inches")),
    size = 5
  )

  expect_s3_class(grob, "grob")
})

test_that("draw_key_swim_arrow handles missing alpha safely", {
  key_data <- data.frame(
    colour = "black",
    fill = "black",
    linewidth = 0.6,
    linetype = 1,
    alpha = NA_real_
  )

  key_data$arrow <- vctrs::new_rcrd(
    list(
      colour = "black",
      fill = "black",
      type = "closed"
    ),
    class = "swim_arrow"
  )

  expect_no_error(
    draw_key_swim_arrow(
      data = key_data,
      params = list(arrow_head_length = grid::unit(0.15, "inches")),
      size = 5
    )
  )
})

test_that("geom_swim_arrow can participate in legend construction", {
  df <- data.frame(
    id = factor(c("A", "B")),
    end_time = c(10, 20)
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(y = id, xend = end_time, arrow = "Continuation"),
      arrow_neck_length = 2,
      show.legend = c(arrow = TRUE)
    ) +
    scale_arrow_discrete(
      limits = "Continuation",
      colours = "black",
      fills = "black",
      types = "closed"
    )

  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("geom_swim_arrow handles mapped aes", {
  df <- data.frame(
    id = factor(c("A", "B")),
    x = c(5, 0),
    end_time = c(10, 20)
  )

  p <- ggplot2::ggplot() +
    geom_swim_arrow(
      data = df,
      ggplot2::aes(y = id, x = x, xend = end_time, arrow = "Continuation"),
      show.legend = c(arrow = TRUE)
    ) +
    scale_arrow_discrete(
      limits = "Continuation",
      colours = "black",
      fills = "black",
      types = "closed"
    )

  expect_no_error(ggplot2::ggplotGrob(p))
})
