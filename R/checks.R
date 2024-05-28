#' @title
#' Check an argument with checkmate
#'
#' @param x An object to check
#' @param arg The name of the argument to include in an error message. Captured
#' by `rlang::caller_arg()` by default
#' @param call the calling environment to use in the error message
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
check_arg_is_character <- wrap_checkmate(check_character)

#' @rdname checkmate
check_arg_is_dataframe <- wrap_checkmate(check_data_frame)

#' @rdname checkmate
check_arg_is_integerish <- wrap_checkmate(check_integerish)

#' @rdname checkmate
check_arg_is_list <- wrap_checkmate(check_list)

#' @rdname checkmate
check_arg_is_logical <- wrap_checkmate(check_logical)

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

#' @title check for unsupported mapping args
#'
#' @description
#' `ggswim()` and `add_marker()` do not support all of the aesthetic mappings
#' their wrapped functions do. By design, this helps `build_ggswim()` work
#' appropriately.
#'
#' @keywords internal
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param unsupported_aes A character vector
#' @param parent_func The function in which this is being called, to be
#' referenced in the message output

check_supported_mapping_aes <- function(mapping,
                                        unsupported_aes,
                                        parent_func) {
  msg <- c(
    "x" = "Unsupported aesthetic mapping params detected: {.code {detected_aes}}",
    "i" = "{.code {parent_func}} does not support {.code {detected_aes}} aesthetic mapping."
  )
  cond_class <- c("ggswim_cond", "unsupported_aes")

  if (any(unsupported_aes %in% names(mapping))) {
    detected_aes <- names(mapping)[names(mapping) %in% unsupported_aes] # nolint: object_usage_linter

    cli_abort(message = msg, call = caller_env(), class = cond_class)
  }
}

#' @title check add_markers for label arguments
#'
#' @description
#' For proper display of label values in the legend, a `color`/`colour` mapping
#' value along with a name specifier is typically needed. It is not mandatory to
#' supply a naming parameter, and a warning will be displayed if labels are used
#' but no naming param is detected.
#'
#' An error will be thrown if a label naming parameter is specified, but no
#' label value parameter is included. Default ggplot2 behavior is to allow this
#' and throw a warning, but we instead wish this to be an error.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#'
#' @keywords internal

check_marker_label_aes <- function(mapping) {
  msg_warn <- c(
    "!" = "Optional label parameters missing.",
    "i" = "Label icons may not appear in the legend without a `label_names` specification."
  )

  msg_error <- c(
    "x" = "Required label parameters missing.",
    "i" = "`label_names` defined without a `label_vals` specification."
  )

  cond_class <- c("ggswim_cond", "marker_label_aes")

  if ("label_vals" %in% names(mapping)) {
    if (!"label_names" %in% names(mapping)) {
      cli_warn(
        message = msg_warn,
        call = caller_env(),
        class = cond_class
      )
    }
  }

  if ("label_names" %in% names(mapping)) {
    if (!"label_vals" %in% names(mapping)) {
      cli_abort(
        message = msg_error,
        call = caller_env(),
        class = cond_class
      )
    }
  }
}


#' @title check geom_swim_arrow for arrow_fill and arrow_type arguments
#'
#' @description
#' Supply users with a warning when `arrow_type` is not "closed" and an `arrow_fill`
#' argument is specified. No error will occur, but nothing will indicate an issue
#' in the output.
#'
#' @inheritParams geom_swim_arrow
#'
#' @keywords internal

check_arrow_fill_type <- function(arrow_type, arrow_fill) {
  msg <- c(
    "!" = "Arrow fill color supplied for an open arrow type.",
    "i" = "Fill colors will only appear for 'closed' arrows types."
  )
  cond_class <- c("ggswim_cond", "arrow_fill_type")

  if (arrow_type != "closed" && !is.null(arrow_fill)) {
    cli_warn(
      message = msg,
      call = caller_env(),
      class = cond_class
    )
  }
}

#' @title check geom_swim_arrow for arrow_neck_length argument
#'
#' @description
#' Supply users with an error when `arrow_neck_length` is not an integer, a
#' symbolic column from the parent dataset, or the default (`NULL`).
#'
#' @inheritParams geom_swim_arrow
#'
#' @keywords internal

check_arrow_neck_length <- function(arrow_neck_length) {
  class_type <- class(arrow_neck_length)

  accepted_class_types <- c("numeric", "name", "NULL")

  msg <- c(
    "x" = "Unsupported data type supplied to .{.code arrow_neck_length}: {class_type}",
    "i" = "Supported class types include numeric values, column names, or `NULL`."
  )
  cond_class <- c("ggswim_cond", "arrow_neck_length_class")

  if (!class_type %in% accepted_class_types) {
    cli_abort(
      message = msg,
      call = caller_env(),
      class = cond_class
    )
  }
}

#' @title check build_ggswim receives a ggswim class object
#'
#' @description
#' Supply users with an error when `build_ggswim()` is used outside of the
#' context of a `ggswim_obj` class object.
#'
#' @param obj The ggplot object passed to the ggswim print method. Ideally, should
#' always have an enforced `ggswim_obj` class type when ggswim is being used.
#'
#' @keywords internal

check_ggswim_obj <- function(obj) {
  msg <- c(
    "x" = "Unsupported object passed to {.code build_ggswim}.",
    "i" = "{.code build_ggswim} only accepts objects of class type 'ggswim_obj'."
  )
  cond_class <- c("ggswim_cond", "ggswim_obj_class")

  if (!"ggswim_obj" %in% class(obj)) {
    cli_abort(
      message = msg,
      call = caller_env(),
      class = cond_class
    )
  }
}

#' @title check for coerced data inside a ggswim function
#'
#' @description
#' Supply users with an error when a coercion to data elements occurs inside of
#' `ggswim()` or `add_marker()` and cannot be rectified with
#' `retrieve_original_aes()`.
#'
#' @details
#' ggswim references internal ggplot layers and any aesthetic mapping
#' required for downstream rendering. If a user applies a coercion in a function,
#' ggswim may not be able to parse the original variable. For example, in
#' `ggswim(mtcars, aes(x = hp, y = cyl, color = factor(disp)`),
#' `rlang::get_expr()` will see the color mapping aesthetic as `factor(disp)`,
#' and not `disp`.
#'
#' @param expr a character vector to test for existence in the names of a
#' dataset
#'
#' @keywords internal

check_coerced_data <- function(expr) {
  # If either no results, or no unique results, throw error
  valid_result <- length(expr) != 1

  msg <- c(
    "x" = "Unsupported ggswim aesthetic mapping detected.",
    "i" = "ggswim could not reconcile coerced aesthetic mapping variables with the supplied dataset."
  )
  cond_class <- c("ggswim_cond", "coerced_vars")

  if (valid_result) {
    cli_abort(
      message = msg,
      call = caller_env(),
      class = cond_class
    )
  }
}

#' @title check for unsupported position args
#'
#' @description
#' `ggswim()` accepts `position` argument `identity` only.
#' Others such as `dodge` and `jitter` are not supported.
#'
#' @keywords internal
#'
#' @param position Position adjustment. ggswim accepts "identity".  Default "identity".
#' @param parent_func The function in which this is being called, to be
#' referenced in the message output

check_supported_position_args <- function(position,
                                          parent_func) {
  msg <- c(
    "x" = "Unsupported position param detected: {.code {position}}",
    "i" = "{.code {parent_func}} does not support {.code {position}} position
    args. Please use one of 'identity' instead."
  )
  cond_class <- c("ggswim_cond", "unsupported_position")

  supported_vals <- c("identity")

  if (!position %in% supported_vals) {
    cli_abort(message = msg, call = caller_env(), class = cond_class)
  }
}

#' @title check for missing aes mappingparams
#'
#' @description
#' Utility check to block users from submitting functions that are missing key
#' parameters.
#'
#' @details
#' A key use of this check is a stopgap for missing `x` and `y` aes parameters
#' which should be valid but don't currently work with our setup.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#' `inherit.aes = TRUE` (the default), it is combined with the default mapping
#' at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param params a vector of params to check for in string format
#' @param parent_func The function in which this is being called, to be
#' referenced in the message output
#'
#' @keywords internal

check_missing_aes_params <- function(mapping,
                                     params,
                                     parent_func) {
  msg <- c(
    "x" = "Missing aes parameters in {.code {parent_func}}",
    "i" = "{params} parameter{?s} are required for {.code {parent_func}}."
  )
  cond_class <- c("ggswim_cond", "missing_aes_params")

  if (!all(params %in% names(mapping))) {
    cli_abort(message = msg, call = caller_env(), class = cond_class)
  }
}

#' @title check for missing params
#'
#' @description
#' Utility check to block users from submitting functions that are missing key
#' parameters. Tests a single parameter for being `NULL`.
#'
#' @param param A parameter to check for `NULL`
#' @param name The name of the parameter checked
#' @param parent_func The function in which this is being called, to be
#' referenced in the message output
#'
#' @keywords internal

check_missing_params <- function(param,
                                 name,
                                 parent_func) {
  msg <- c(
    "x" = "Missing parameters in {.code {parent_func}}",
    "i" = "Parameter `{name}` required for {.code {parent_func}}."
  )
  cond_class <- c("ggswim_cond", "missing_params")

  if (is.null(param)) {
    cli_abort(message = msg, call = caller_env(), class = cond_class)
  }
}
