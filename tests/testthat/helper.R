# This script intended to set up often referenced objects for the tes suite

simple_plot <- function() {
  all_events <- bind_rows(
    infusion_events,
    end_study_events,
  )

  ggplot() +
    geom_swim_lane(
      data = patient_data,
      aes(
        x = .data$start_time, xend = .data$end_time, y = .data$pt_id,
        colour = .data$disease_assessment
      )
    ) +
    geom_swim_marker(
      data = all_events,
      aes(x = .data$time_from_initial_infusion, y = .data$pt_id, marker = .data$label),
      size = 4,
    ) +
    with(all_events, scale_marker_discrete(glyphs = glyph, colours = colour, limits = label, name = "Marker")) +
    ggplot2::scale_color_brewer(
      name = "Lanes",
      palette = "Set1"
    ) +
    theme_ggswim()
}

sample_arrow_data <- function() {
  patient_data |>
    dplyr::left_join(
      end_study_events |>
        dplyr::select(pt_id, label), # nolint: object_usage_linter
      by = "pt_id"
    ) |>
    dplyr::select(pt_id, end_time, label) |> # nolint: object_usage_linter
    dplyr::filter(.by = pt_id, end_time == max(end_time)) |> # nolint: object_usage_linter
    dplyr::filter(!is.na(label)) |>
    unique()
}
