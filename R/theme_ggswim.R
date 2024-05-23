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
#' p <- ggplot2::ggplot(data = patient_data) +
#'   geom_swim_lane(
#'     mapping = aes(
#'       x = start_time,
#'       y = pt_id,
#'       xend = end_time,
#'       color = disease_assessment
#'     )
#'   ) +
#'   ggplot2::scale_color_brewer(name = "Lanes", palette = "Set1") +
#'   new_scale_color() +
#'   geom_swim_point(
#'     data = infusion_events,
#'     mapping = aes(x = time_from_initial_infusion,
#'     y = pt_id, color = infusion_type), # nolint: object_usage_linter
#'     size = 5
#'   ) +
#'   ggplot2::scale_color_manual(name = "Markers", values = c("red", "green"))
#'
#' p +
#'   theme_ggswim()
theme_ggswim <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Title and subtitle
      plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 10)),

      # Caption
      plot.caption = element_text(size = 10, hjust = 1),

      # Axis text
      axis.text = element_text(size = 10),

      # Axis titles
      axis.title = element_text(size = 12, face = "bold"),

      # Legend
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),

      # Axis line
      axis.line = element_line(
        color = "steelblue",
        linewidth = .5,
        arrow = arrow(
          type = "closed",
          length = unit(0.2, "inches")
        )
      ),

      # Remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
