#' @title Plot swimmer data
#'
#' @description
#' Plot swimmer data.
#'
#' @param x A data object of type `swim_tbl` as supplied by `streamline()`
#' @param title the plot title
#' @param xlab the x-axis label
#' @param ylab the y-axis label
#' @param ... arguments passed to ...
#'
#' @returns a ggplot2 figure
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw scale_x_continuous
#' labs guides theme guide_legend scale_color_manual element_text element_blank
#' element_rect geom_text
#' @importFrom cli cli_abort
#'
#' @export

ggswim <- function(x,
                   title = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   ...) {
  # check inputs ---------------------------------------------------------------
  check_arg_is_swim_tbl(x)

  # assign common vars ---------------------------------------------------------
  df <- x$data
  markers <- x$markers
  id <- x$id
  time <- x$time
  lanes <- x$lanes

  # Define initial gg object and apply lines colored by lanes ------------------
  gg <- df |>
    ggplot(aes(x = !!time, y = !!id, group = !!id)) +
    geom_line(aes(col = x$data$lane_col),
              linewidth = 1.8) +
    geom_text(aes(label = markers[marker_col],
                  col = marker_col),
              family = "Arial Unicode MS",
              size = 10, show.legend = TRUE) +
    ggplot2::theme_minimal() +
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
