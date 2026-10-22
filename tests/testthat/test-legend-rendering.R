test_that("marker and arrow legends render with swimmer lanes", {
  data <- data.frame(
    start = c(0, 0),
    end = c(8, 8),
    time = c(3, 5),
    id = c("A", "B"),
    event = c("Response", "Review"),
    arrow_width = factor(
      c("Standard", "Emphasized"),
      levels = c("Standard", "Emphasized")
    )
  )
  plot <- ggplot2::ggplot() +
    geom_swim_lane(
      data = data,
      mapping = ggplot2::aes(x = .data$start, xend = .data$end, y = .data$id),
      colour = "grey65"
    ) +
    geom_swim_marker(
      data = data,
      mapping = ggplot2::aes(
        x = .data$time,
        y = .data$id,
        marker = .data$event
      ),
      size = 5
    ) +
    scale_marker_discrete(
      glyphs = c("+", "x"),
      colours = c("firebrick", "steelblue"),
      limits = c("Response", "Review"),
      name = "Event"
    ) +
    geom_swim_arrow(
      data = data,
      mapping = ggplot2::aes(
        xend = .data$end,
        y = .data$id,
        linewidth = .data$arrow_width
      ),
      arrow_neck_length = 1,
      arrow_colour = "grey20",
      arrow_fill = "grey20",
      arrow_type = "closed",
      show.legend = c(linewidth = TRUE, marker = FALSE)
    ) +
    ggplot2::scale_linewidth_manual(
      values = c(Standard = 0.6, Emphasized = 1.2),
      name = "Arrow width"
    ) +
    ggplot2::theme_minimal()

  vdiffr::expect_doppelganger("marker and arrow legends with lanes", plot)
})
