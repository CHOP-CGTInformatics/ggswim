#' @keywords internal
#' @aliases ggswim-package
#' @importFrom ggplot2 aes layer_data ggplot scale_color_manual geom_segment arrow
#'   %+replace% theme_minimal theme element_text element_blank margin unit
#'   element_line element_rect unit ggproto layer scale_colour_manual
#'   Geom GeomSegment GeomText discrete_scale draw_key_text
#' @importFrom dplyr case_when left_join arrange bind_rows select arrange any_of
#' mutate pull if_else filter distinct
#' @importFrom cli cli_abort cli_vec cli_warn
#' @importFrom rlang caller_arg caller_env as_label is_atomic get_expr .data
#'   is_empty := enquo quo_is_symbolic quo list2 try_fetch eval_tidy %||%
#' @importFrom purrr map_lgl walk discard
#' @importFrom stats setNames
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble tribble enframe
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
