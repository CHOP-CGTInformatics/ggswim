#' @title ggswim Theme
#'
#' @description
#' Alternative themes for ggswim plots:
#'
#' - `theme_ggswim()`: Builds on `theme_minimal()` with removal of grid lines and some aesthetic updates.
#'
#' @returns a ggplot2 theme
#'
#' @export
#'
#' @examples
#' ggswim(
#'   data = patient_status,
#'   mapping = aes(
#'     x = value,
#'     y = subject_id,
#'     fill = cohort
#'   )
#' ) +
#'   theme_ggswim()
theme_ggswim <- function() {
  font <- "serif"

  theme_minimal() %+replace%

    theme(
      # Grid Elements ----
      axis.ticks = element_line(color = "steelblue3", linewidth = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line = element_line(
        color = "steelblue3", linewidth = 1,
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
