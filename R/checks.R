#' @title
#' Check an argument with checkmate
#'
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg
#'
#' @param x An object to check
#' @param arg The name of the argument to include in an error message. Captured
#' by `rlang::caller_arg()` by default
#' @param call the calling environment to use in the error message
#' @param req_cols required fields for `check_arg_is_supertbl()`
#' @param ... additional arguments passed on to checkmate
#'
#' @return
#' `TRUE` if `x` passes the checkmate check. An error otherwise with the name of
#' the checkmate function as a `class`
#'
#' @name checkmate
#' @keywords internal
NULL

# Function factory to wrap checkmate functions
#' @importFrom rlang caller_arg caller_env
#' @importFrom cli cli_abort
#' @noRd
wrap_checkmate <- function(f) {
  error_class <- caller_arg(f)

  function(x, ..., arg = caller_arg(x), call = caller_env()) {
    out <- f(x, ...)

    if (isTRUE(out)) {
      return(TRUE)
    }

    cli_abort(
      message = c(
        "x" = "You've supplied {.code {format_error_val(x)}} for {.arg {arg}} which is not a valid value",
        "!" = "{out}"
      ),
      class = c(error_class, "ggswim_cond"),
      call = call
    )
  }
}

#' @rdname checkmate
#' @importFrom cli cli_abort
#' @importFrom rlang caller_env caller_arg
check_arg_is_swim_tbl <- function(x,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
  # shared data for all messages
  msg_x <- "You've supplied {.code {format_error_val(x)}} for {.arg {arg}} which is not a valid value"
  msg_info <- "{.arg {arg}} must be a {.pkg ggwim} swim_tbl, generated using {.code streamline()}"
  msg_class <- c("check_swim_tbl", "ggswim_cond")

  if (!inherits(x, "swim_tbl")) {
    cli_abort(
      message = c(
        "x" = msg_x,
        "!" = "Must be of class {.cls swim_tbl}",
        "i" = msg_info
      ),
      class = msg_class,
      call = call
    )
  }
  return(TRUE)
}

#' @rdname checkmate
#' @importFrom checkmate check_character
check_arg_is_character <- wrap_checkmate(check_character)

#' @rdname checkmate
#' @importFrom checkmate check_logical
check_arg_is_logical <- wrap_checkmate(check_logical)

#' @rdname checkmate
#' @importFrom checkmate check_data_frame
check_arg_is_dataframe <- wrap_checkmate(check_data_frame)

#' @title
#' Format value for error message
#'
#' @param x value to format
#'
#' @return
#' If x is atomic, x with cli formatting to truncate to 5 values. Otherwise,
#' a string summarizing x produced by as_label
#'
#' @importFrom rlang as_label is_atomic
#' @importFrom cli cli_vec
#'
#' @keywords internal
format_error_val <- function(x) {
  if (is_atomic(x)) {
    out <- cli_vec(x, style = list("vec-trunc" = 5, "vec-last" = ", "))
  } else {
    out <- as_label(x)
  }
  out
}