#' @title Streamline data for ggswim plotting
#'
#' @description
#' Prepare a dataset for use with `ggswim()` and ensure proper elements are made
#' available.
#'
#' @details
#' Requirements for a dataset include an identifiable primary key column,
#' a time column, and any number of binary indicator columns.
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
#'
#' @returns a swim_tbl object
#'
#' @importFrom rlang enquo get_expr
#' @importFrom stats reorder
#' @importFrom tidyr fill
#'
#' @export

streamline <- function(df,
                       id,
                       time,
                       events,
                       reference_event,
                       markers = NULL,
                       lanes) {
  # Convert lanes to ordered factor
  if (is.null(names(lanes))){
    lanes <- factor(unlist(lanes), levels = unlist(lanes), ordered = TRUE)
    lane_colors <- NULL
  } else {
    lane_colors <- unlist(lanes)
    lanes <- factor(names(lanes), levels = names(lanes), ordered = TRUE)
  }

  # Check inputs ---------------------------------------------------------------
  # TODO: Add checks for other args. To access "name" args, use df[[*]]
  # TBD on how best to access "call" args (i.e. `markers`)
  check_arg_is_dataframe(df)

  # Group subject vars and assign max time -------------------------------------
  # Replicate dplyr::group_by() using base R
  # lapply/split separates into list elements per `id`
  grouped <- lapply(split(df, df[[id]]), lanes, FUN = function(group_df, lanes = lanes) {
    # Perform operations on each group
    # i.e., calculate the max/min of 'time' column
    min_value <- min(group_df[time], na.rm = TRUE)
    max_value <- max(group_df[time], na.rm = TRUE)

    # Create a new column in the group_df with the max value
    group_df$min_time <- min_value
    group_df$max_time <- max_value

    # Create lane column
    group_df$lane_col <- group_df$event
    group_df$lane_col[!group_df$lane_col %in% lanes] <- NA

    group_df <- group_df |>
      # Fill down first, then up
      fill(lane_col, .direction = "down") # nolint: object_usage_linter

    # Create marker column
    group_df$marker_col <- group_df$event
    group_df$marker_col[group_df$marker_col %in% lanes] <- NA

    # Return the modified group_df
    group_df
  })

  # Combine the results back into a single data frame --------------------------
  result <- do.call(rbind, grouped)
  # Convert `id` to factor
  result[[id]] <- reorder(
    factor(result[[id]]), result$max_time
  )

  # Capture all vars of interest in a list and return as a swim_tbl object -----
  # Create marker levels, combine with lanes levels for later ggswim legend support
  marker_levels <- factor(names(markers), levels = names(markers), ordered = TRUE)

  out <- list(data = result,
              id = id,
              time = time,
              markers = markers,
              reference_event = reference_event,
              lanes = lanes,
              lane_colors = lane_colors,
              event_levels = factor(result$event, levels = c(levels(lanes), levels(marker_levels)), ordered = TRUE)
  )

  as_swim_tbl(out)
}

#' @title
#' Add swim_tbl S3 class
#'
#' @param x an object to class
#'
#' @return
#' The object with `swim_tbl` S3 class
#'
#' @keywords internal
#'
as_swim_tbl <- function(x) {
  class(x) <- c("swim_tbl", class(x))
  x
}
