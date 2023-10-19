#' @title ggswim Theme
#'
#' @description
#' An alternative theme for ggswim plots.
#'
#' @export
#'
#' @importFrom ggplot2 %+replace% theme_minimal theme element_text
#' element_blank margin element_line element_rect

theme_ggswim <- function() {
  font <- "serif"

  theme_minimal() %+replace%

    theme(
      # Grid Elements ----
      axis.ticks = element_line(color = "steelblue2", size = 1),
      panel.grid.major.x = element_line(color = "gray60", size = 0.3),
      panel.grid.minor.x = element_line(color = "gray80", size = 0.1),
      panel.grid.major.y = element_line(color = "gray60", size = 0.3),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(
        color = "steelblue3", size = 1,
        arrow = arrow(
          type = "closed",
          length = unit(0.08, "inches")
        )
      ),


      # Text Elements ----
      plot.title = element_text(
        family = font,
        size = 16,
        face = "bold",
        hjust = .5,
        vjust = 2
      ),
      plot.subtitle = element_text(
        family = font,
        size = 12
      ),
      plot.caption = element_text(
        family = font,
        size = 9,
        hjust = 1
      ),
      axis.title = element_text(
        face = "bold",
        family = font,
        size = 12
      ),
      axis.text = element_text(
        family = font,
        size = 9
      ),

      # Legend Elements ----
      legend.background = element_rect(
        fill = "white",
        linewidth = 4,
        colour = "white"
      ),
      legend.text = element_text(
        family = font
      ),
      legend.title = element_text(
        hjust = 0,
        family = font,
        face = "bold"
      )
    )
}
