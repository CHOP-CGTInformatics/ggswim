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
#' @param df a dataframe prepared for use with `streamline()`
#' @param id the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time the the x-axis variable of the swimmer plot, typically a
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
#'
#' @export

streamline <- function(df,
                       id,
                       time,
                       events,
                       reference_event,
                       markers = NULL,
                       lanes) {
  # Capture variables as expressions, allowing for piping in API ---------------
  variables <- c("id", "time", "events", "reference_event")

  for (variable in variables) {
    assign(variable, eval(parse(text = paste0("enquo(", variable, ") |> get_expr()"))))
  }

  # markers <- paste0(markers[-1]) # [-1] due to vector of type `sym` includes "c()"
  # lanes <- paste0(lanes[-1]) # [-1] due to vector of type `sym` includes "c()"

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
      tidyr::fill(lane_col, .direction = "down") # Fill down first, then up

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
  out <- list(data = result,
              id = id,
              time = time,
              markers = markers,
              reference_event = reference_event,
              lanes = lanes)

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
