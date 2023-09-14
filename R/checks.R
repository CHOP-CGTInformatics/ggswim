#' @title
#' Check an argument with checkmate
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
#' @keywords interal
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
  msg = c("x" = "Unsupported aesthetic mapping params detected: {.code {detected_aes}}",
          "i" = "{.code {parent_func}} does not support {.code {detected_aes}} aesthetic mapping.")
  cond_class = c("ggswim_cond", "unsupported_aes")

  if (any(unsupported_aes %in% names(mapping))) {
    detected_aes <- names(mapping)[names(mapping) %in% unsupported_aes]

    cli_abort(message = msg, call = caller_env(), class = cond_class)
  }
}

#' @title Check for plot legend discrepancies due to missing data
#'
#' @description
#' There is a known issue with ggswim `add_markers()` where missing data in prior
#' layers can cause issues when compared to non-missing data in subsequent layers.
#' Due to the way ggswim handles manipulation of the legend to allow for combination
#' of various layer types under the "colour" layer, NA values must be dropped from
#' the legend display if they are detected in the data but not in the `ggswim_obj`.
#'
#' This error occurs due to a mismatch in expected values supplied to the `guide()`
#' via the `override` reference list.
#'
#' @keywords internal
#'
#' @param ggswim_obj A ggswim object
#' @param override An internal list object responsible for legend `guide()` rebuilds
#'
#' @returns A boolean

check_for_na_legend_discrepancy <- function(ggswim_obj, override) {
  gg_built <- ggplot_build(ggswim_obj)

  for (i in seq_along(gg_built$plot$scales$scales)) {
    # Non aes scales contain multiple elements, but aes scales only have the one (colour, fill, shape, etc.)
    # For this check, we are interested in the colour scale
    current_scale <- gg_built$plot$scales$scales[[i]]
    is_colour_scale <- length(current_scale$aesthetics) == 1 && current_scale$aesthetics == "colour"

    if (is_colour_scale) {
      # Check that NA exists in list of overrides, but does not exist in the plot itself
      # Use na.value for the default NA colour value (i.e. "grey50")
      plot_legend_missing_na <- any(is.na(override$colour$colour_mapping)) &&
        !any(is.na(current_scale$palette.cache)) &&
        !any(current_scale$palette.cache == current_scale$na.value)
    }
  }

  if (plot_legend_missing_na) {
    cli_warn(
      message = c("!" = "Missing data detected that has been dropped from the legend display.",
                  "i" = "Missing data may still appear in the {.code ggswim} plot."),
      call = caller_env(),
      class = c("ggswim_cond", "na_legend_discrepancy")
    )
  }

  plot_legend_missing_na
}
