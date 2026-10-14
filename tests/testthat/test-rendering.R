panel_children <- function(plot) {
  grob <- ggplot2::ggplotGrob(plot)
  grob$grobs[[which(grob$layout$name == "panel")]]$children
}

test_that("inherited lane endpoints and fixed styling reach the panel", {
  withr::local_pdf(NULL)
  data <- data.frame(start = c(0, 2), end = c(5, 8), id = c("A", "B"))
  plot <- ggplot2::ggplot(data, aes(x = start, xend = end, y = id)) +
    geom_swim_lane(colour = "red", linewidth = 2)
  built <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(built$x, data$start)
  expect_equal(built$xend, data$end)
  segments <- Filter(function(x) inherits(x, "segments"), panel_children(plot))
  expect_length(segments, 1)
  expect_equal(segments[[1]]$y0, segments[[1]]$y1)
  expect_equal(grDevices::col2rgb(segments[[1]]$gp$col),
               grDevices::col2rgb(rep("red", 2)))
})

test_that("explicit marker styles reach both panel and legend", {
  withr::local_pdf(NULL)
  data <- data.frame(time = 1:2, id = c("A", "B"), event = c("B", "A"))
  scale <- scale_marker_discrete(
    glyphs = c("+", "x"), colours = c("red", "blue"), limits = c("A", "B")
  )
  plot <- ggplot2::ggplot(data, aes(x = time, y = id, marker = event)) +
    geom_swim_marker() + scale
  text <- Filter(function(x) inherits(x, "text"), panel_children(plot))
  expect_length(text, 1)
  expect_equal(text[[1]]$label, c("x", "+"))
  expect_equal(grDevices::col2rgb(text[[1]]$gp$col),
               grDevices::col2rgb(c("blue", "red")))
  key <- GeomSwimMarker$draw_key(
    data.frame(marker = scale$palette(2)[1], size = 4, alpha = 1), list(), 5
  )
  if (inherits(key, "gTree")) key <- key$children[[1]]
  expect_equal(key$label, "+")
  expect_equal(grDevices::col2rgb(key$gp$col), grDevices::col2rgb("red"))
})

test_that("inherited arrows preserve explicit extent and scaled styling", {
  withr::local_pdf(NULL)
  data <- data.frame(start = c(5, 8), end = c(6, 9), id = c("A", "B"))
  plot <- ggplot2::ggplot(data, aes(x = start, xend = end, y = id)) +
    geom_swim_arrow(aes(arrow = "Ongoing"), arrow_neck_length = 100) +
    scale_arrow_discrete(
      limits = "Ongoing", colours = "red", fills = "blue", types = "closed"
    )
  built <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(built$x, data$start)
  expect_equal(built$xend, data$end)
  segments <- Filter(function(x) inherits(x, "segments"), panel_children(plot))
  expect_equal(grDevices::col2rgb(segments[[1]]$gp$col),
               grDevices::col2rgb(rep("red", 2)))
  expect_equal(grDevices::col2rgb(segments[[1]]$gp$fill),
               grDevices::col2rgb(rep("blue", 2)))
  key <- draw_key_swim_arrow(
    data.frame(arrow = built$arrow[1], linewidth = 0.5, alpha = 1), list(), 5
  )
  expect_equal(grDevices::col2rgb(key$gp$col), grDevices::col2rgb("red"))
  expect_equal(grDevices::col2rgb(key$gp$fill), grDevices::col2rgb("blue"))
})

test_that("missing event categories are omitted with explicit limits", {
  scale <- scale_marker_discrete(glyphs = "+", colours = "red", limits = "A")
  scale$train(c("A", NA, "outside"))
  expect_equal(vctrs::vec_detect_missing(scale$map(c("A", NA, "outside"))),
               c(FALSE, TRUE, TRUE))
})

test_that("lane and arrow rendering remains stable", {
  # No text or legends: this snapshot is independent of installed icon fonts.
  plot <- ggplot2::ggplot(data.frame(start = 0, end = 10, id = 1)) +
    geom_swim_lane(aes(x = start, xend = end, y = id), colour = "grey50") +
    geom_swim_arrow(aes(xend = end, y = id), arrow_neck_length = 2,
                    arrow_colour = "red", arrow_fill = "blue") +
    ggplot2::theme_void()
  vdiffr::expect_doppelganger("lane and continuation arrow", plot)
})
