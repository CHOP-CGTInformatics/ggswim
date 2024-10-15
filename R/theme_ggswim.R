#' @title
#' Apply custom theme styling for ggplot2 plots
#'
#' @description
#' This function applies custom styling to various elements of ggplot2 plots,
#' including title, subtitle, caption, axis text, axis titles, and legend text
#' and legend titles.
#'
#' @details
#' This function builds upon the `theme_minimal()` function in ggplot2 but overrides
#' specific theme elements to provide a customized look and feel for plots.
#'
#' @param base_size The base font size to use for the plot elements. Default is 12.
#' @param base_family The base font family to use for the plot elements. Default is "".
#'
#' @return A ggplot2 theme object.
#'
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot() +
#'   geom_swim_lane(
#'     data = patient_data,
#'     mapping = aes(
#'       x = start_time,
#'       xend = end_time,
#'       y = pt_id,
#'       colour = disease_assessment
#'     ),
#'     linewidth = 3
#'   )
#'
#' p +
#'   theme_ggswim()
#'
#' p +
#'   theme_ggswim_dark()
theme_ggswim <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Title and subtitle
      plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 10), colour = "#000000"),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 10), colour = "#000000"),

      # Caption
      plot.caption = element_text(size = 10, hjust = 1),

      # Axis text
      axis.text = element_text(size = 10, face = "bold", colour = "#000000"),
      axis.text.x = element_text(margin = margin(t = 10)),
      axis.text.y = element_text(margin = margin(r = 10)),

      # Axis titles
      axis.title = element_text(size = 12, face = "bold", colour = "#000000"),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),

      # Axis tick marks
      axis.ticks = element_line(linewidth = 0.75, colour = "steelblue"),
      axis.ticks.length = unit(0.3, "cm"),

      # Legend
      legend.text = element_text(size = 10, colour = "#000000"),
      legend.title = element_text(size = 12, face = "bold", colour = "#000000"),

      # Axis line
      axis.line = element_line(
        color = "steelblue",
        linewidth = 1
      ),

      # Remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#' @rdname theme_ggswim
#' @export
theme_ggswim_dark <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Background
      plot.background = element_rect(fill = "#232234", color = NA),
      panel.background = element_rect(fill = "#232234", color = NA),

      # Title and subtitle
      plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 10), colour = "#F0F0F0"),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 10), colour = "#F0F0F0"),

      # Caption
      plot.caption = element_text(size = 10, hjust = 1, colour = "#F0F0F0"),

      # Axis text
      axis.text = element_text(size = 10, face = "bold", colour = "#F0F0F0"),
      axis.text.x = element_text(margin = margin(t = 10), colour = "#F0F0F0"),
      axis.text.y = element_text(margin = margin(r = 10), colour = "#F0F0F0"),

      # Axis titles
      axis.title = element_text(size = 12, face = "bold", colour = "#F0F0F0"),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10), colour = "#F0F0F0"),
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10), colour = "#F0F0F0"),

      # Axis tick marks
      axis.ticks = element_line(linewidth = 0.75, colour = "#F0F0F0"),
      axis.ticks.length = unit(0.3, "cm"),

      # Legend
      legend.text = element_text(size = 10, colour = "#F0F0F0"),
      legend.title = element_text(size = 12, face = "bold", colour = "#F0F0F0"),

      # Axis line
      axis.line = element_line(
        color = "#F0F0F0", # Orange axis line
        linewidth = 1
      ),

      # Remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
