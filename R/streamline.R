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
#' @param subject_var the y-axis variable of a swimmer plot, typically a unique
#' subject or record identification column
#' @param time_var the the x-axis variable of the swimmer plot, typically a
#' function of time
#' @param markers a vector of columns that will comprise point markers on the
#' swimmer plot
#' @param class_status Columns that indicate line changes, i.e. color changes
#' for individual swim lanes. Optional, default `NULL`.
#'
#' @returns a swim_tbl object
#'
#' @importFrom rlang enquo get_expr
#' @importFrom stats reorder
#'
#' @export

streamline <- function(df,
                       subject_var,
                       time_var,
                       markers,
                       class_status = NULL) {
  # Capture variables as expressions, allowing for piping in API ---------------
  subject_var <- enquo(subject_var) |>
    get_expr()
  time_var <- enquo(time_var) |>
    get_expr()
  markers <- enquo(markers) |>
    get_expr()
  class_status <- enquo(class_status) |>
    get_expr()

  # Check inputs ---------------------------------------------------------------
  # TODO: Add checks for other args. To access "name" args, use df[[*]]
  # TBD on how best to access "call" args (i.e. `markers`)
  check_arg_is_dataframe(df)

  # Expand columns to assign day of marker occurrence --------------------------
  time_vec <- paste0(markers[-1], "_timepoint") # [-1] due to vector of type `sym` includes "c()"

  # Add named columns with empty rows to dataframe
  df <- df |>
    add_columns(time_vec)

  # Conditionally update values in time_vec with time of occurrence if value in original col != 0
  for (df_rows in seq_len(nrow(df))) {
    for (marker in as.character(markers[-1])) { # [-1] due to vector of type `sym` includes "c()"
      if (df[df_rows, marker] != 0) {
        df[df_rows, paste0(marker, "_timepoint")] <- df[df_rows, time_var]
      }
    }
  }

  # Group subject vars and assign max time -------------------------------------
  # Replicate dplyr::group_by() using base R
  # lapply/split separates into list elements per `subject_var`
  # Then, max_time is created with the max of the split `time_var` column
  grouped <- lapply(split(df, df[[subject_var]]), function(group_df) {
    # Perform operations on each group
    # i.e., calculate the max of 'time_var' column
    max_value <- max(group_df[time_var], na.rm = TRUE)

    # Create a new column in the group_df with the max value
    group_df$max_time <- max_value

    # Return the modified group_df
    group_df
  })

  # Combine the results back into a single data frame --------------------------
  result <- do.call(rbind, grouped)
  # Convert `subject_var` to factor
  result[[subject_var]] <- reorder(
    factor(result[[subject_var]]), result$max_time
  )

  # Capture all vars of interest in a list and return as a swim_tbl object -----
  out <- list(data = result,
              markers = time_vec,
              subject_var = subject_var,
              time_var = time_var,
              class_status = class_status)

  as_swim_tbl(out)
}

#' @title Add empty columns based on supplied day_vector
#'
#' @description
#' Add empty columns to a dataframe based on supplied day_vector
#'
#' @param df a dataframe
#'
#' @returns a dataframe
#'
#' @keywords internal

add_columns <- function(df, columns) {
  within(df, {
    for (col in columns) {
      assign(col, NA)  # Assign values to new column
    }
    col <- NULL
  })
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
