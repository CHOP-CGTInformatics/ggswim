#' @title Retrieve original vars from coerced vars
#'
#' @description
#' Detect instances where users manipulate `aes()` names that ggswim
#' requires to access and identify layer types downstream. In instances where
#' such a coercion is detected, attempt to retrieve the original `aes()` name.
#'
#' @details
#' ggswim references internal ggplot layers and any aesthetic mapping
#' required for downstream rendering. If a user applies a coercion in a function,
#' ggswim may not be able to parse the original variable. For example, in
#' `ggswim(mtcars, aes(x = hp, y = cyl, color = factor(disp)`),
#' `rlang::get_expr()` will see the color mapping aesthetic as `factor(disp)`,
#' and not `disp`.
#'
#' @param data the data responsible for the current layer
#' @param aes_mapping a list of mapping data (i.e. `unlist(mapping)`)
#' @param aes_var the aesthetic variable to test for (ex: `color`, `shape`)
#'
#' @returns The original variable name as a character string
#'
#' @keywords internal

retrieve_original_aes <- function(data, aes_mapping, aes_var) {
  layer_aes <- aes_mapping[[aes_var]] |>
    get_expr() |>
    paste()
  original_var <- layer_aes[layer_aes %in% names(data)]

  # If original var cannot be validated, throw error
  check_coerced_data(expr = original_var)

  original_var
}
