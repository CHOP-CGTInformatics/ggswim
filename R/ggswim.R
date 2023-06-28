#' @title Plot swimmer data
#'
#' @description
#' Plot swimmer data.
#'
#' @param df a dataframe prepared for use with `ggswim()`
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time the x-axis variable of the swimmer plot, typically a
#' function of time
#' @param events the column that will supply definitions for the `reference_event`,
#' `markers`, and `lanes`
#' @param reference_event a character string found in `events` that establishes
#' the time-zero reference point for the x-axis
#' @param markers A character vector that will comprise point markers on the
#' swimmer plot. Optional, default `NULL`
#' @param lanes Columns that indicate line changes, i.e. color changes
#' for individual swim lanes.
#' @param lane_colors a vector of character strings to assign to the `lanes`,
#' written in the same order. Optional, if left `NULL`, default colors applied
#' @param title the plot title
#' @param xlab the x-axis label
#' @param ylab the y-axis label
#'
#' @returns a ggplot2 figure
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw scale_x_continuous
#' labs guides theme guide_legend scale_color_manual element_text element_blank
#' element_rect geom_label theme_minimal
#' @importFrom cli cli_abort
#'
#' @export

ggswim <- function(df,
                   id,
                   time,
                   events,
                   reference_event,
                   markers,
                   lanes,
                   lane_colors = NULL,
                   title = NULL,
                   xlab = NULL,
                   ylab = NULL) {

  # Streamline the dataframe ---------------------------------------------------
  # Capture variables as expressions, allowing for piping in API ---------------
  variables <- c("id", "time", "events", "reference_event")

  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  swim_tbl <- streamline(df = df,
                         id = id,
                         time = time,
                         events = events,
                         reference_event = reference_event,
                         markers = markers,
                         lanes = lanes)

  # check inputs ---------------------------------------------------------------

  # assign common vars ---------------------------------------------------------
  df <- swim_tbl$data
  markers <- swim_tbl$markers
  id <- swim_tbl$id
  time <- swim_tbl$time
  lanes <- swim_tbl$lanes
  if (!is.null(lane_colors)) {
    names(lane_colors) <- lanes
  }


  # Define initial gg object and apply lines colored by lanes ------------------
  gg <- df |>
    ggplot(aes(x = !!time, y = !!id, group = !!id)) +
    geom_line(aes(col = swim_tbl$data$lane_col),
              linewidth = 1.8)

  # Conditionally apply lane colors
  # Needs to be done *before* application of markers
  if (!is.null(lane_colors)) {
    gg <- gg +
      scale_color_manual(values = lane_colors)
  }

  gg <- gg +
      geom_label(
      aes(
        label = markers[marker_col], # nolint: object_usage_linter
        col = marker_col # nolint: object_usage_linter
      ),
      label.size = NA, fill = NA) +
    theme_minimal() +
    labs(x = xlab, y = ylab, title = title)

  gg

  # TODO: Automate theme, legend/guide, labs, scale
}


#' @title apply an arbitrary number of `geom_point()`'s to a ggplot
#'
#' @description This function applies any number of necessary `geom_point()`
#' objects to a `ggplot2` chain as declared in `markers`.
#'
#' @return a ggplot object
#'
#' @param plot a ggplot2 object
#' @param markers a vector of columns that will comprise point markers on the
#' swimmer plot
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#'
#' @importFrom ggplot2 layer
#'
#' @keywords internal

apply_geom_points <- function(plot, markers, id) {
  # Determine the number of geom_point commands based on the length of markers
  num_geoms <- length(markers)

  # Create a list of geom_point layers
  layers <- lapply(seq_len(num_geoms), function(i) {
    layer(data = NULL, geom = "point",
          stat = "identity",
          position = "identity",
          mapping = aes(x = !!rlang::sym(markers[[i]]),
                        y = paste0(!!id),
                        col = paste(markers[[i]])))
  })

  # Add all layers to the existing plot using Reduce and the + operator
  plot <- Reduce(`+`, layers, init = plot)

  # Return the final plot with all layers applied
  return(plot)
}
