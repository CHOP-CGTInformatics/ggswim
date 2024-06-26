#' @keywords internal
#' @aliases ggswim-package
#' @importFrom ggplot2 aes geom_point geom_label layer_data ggplot geom_col
#'   guides guide_legend scale_color_manual geom_segment arrow unit
#'   %+replace% theme_minimal theme element_text element_blank margin
#'   element_line element_rect get_guide_data unit ggproto layer ggplot_build
#'   Geom GeomSegment GeomLabel GeomPoint ggplot_add
#' @importFrom dplyr case_when left_join arrange bind_rows select arrange any_of
#' mutate pull if_else filter
#' @importFrom cli cli_abort cli_vec cli_warn
#' @importFrom rlang caller_arg caller_env as_label is_atomic get_expr .data
#'   is_empty := enquo quo_is_symbolic quo list2 try_fetch eval_tidy
#' @importFrom purrr map_lgl walk discard
#' @importFrom checkmate check_logical check_list check_integerish check_data_frame check_character
#' @importFrom stats setNames
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom tibble tibble tribble enframe
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
