# This script intended to set up often referenced objects for the tes suite

simple_plot <- function() {
  patient_data |>
    ggplot() +
    geom_swim_lane(mapping = aes(x = start_time, y = pt_id, xend = end_time, color = disease_assessment)) + # nolint: object_usage_linter
    ggplot2::scale_color_brewer(name = "Lanes", palette = "Set1") +
    new_scale_color() +
    geom_swim_point(
      data = infusion_events,
      mapping = aes(x = time_from_initial_infusion, y = pt_id, color = infusion_type), # nolint: object_usage_linter
      size = 5
    ) +
    ggplot2::scale_color_manual(name = "Markers", values = c("red", "green")) +
    new_scale_color() +
    geom_swim_label(
      data = end_study_events,
      mapping = aes(x = time_from_initial_infusion, y = pt_id, # nolint: object_usage_linter
                    label_vals = end_study_label, label_names = end_study_name), # nolint: object_usage_linter
      label.size = NA, size = 5
    ) +
    ggplot2::scale_color_brewer(name = "Labels", palette = 1) +
    theme_ggswim()
}

no_label_plot <- function() {
  patient_data |>
    ggplot() +
    geom_swim_lane(mapping = aes(x = start_time, y = pt_id, xend = end_time, color = disease_assessment)) + # nolint: object_usage_linter
    ggplot2::scale_color_brewer(name = "Lanes", palette = "Set1") +
    new_scale_color() +
    geom_swim_point(
      data = infusion_events,
      mapping = aes(x = time_from_initial_infusion, y = pt_id, color = infusion_type), # nolint: object_usage_linter
      size = 5
    ) +
    ggplot2::scale_color_manual(name = "Markers", values = c("red", "green"))
}
