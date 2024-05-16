#' @title Apply ggswim fixes and display updates
#'
#' @description
#' Users should generally never have to call this function, but it is used to
#' invoke final changes before plot rendering and a custom print method via
#' [print.ggswim_obj()].
#'
#' @details
#' In its current state, `build_ggswim()` can only work with a pre-rendered
#' ggswim plot object, therefore it cannot be added to the `+` operator chain.
#'
#' @param ggswim_obj A ggswim object
#'
#' @returns A ggplot2 object
#' @export
#'
#' @examples
#' ggswim_obj <- ggplot2::ggplot(data = patient_data) +
#'  geom_swim_lane(
#'   mapping = aes(x = start_time,
#'                 y = pt_id,
#'                 xend = end_time,
#'                 color = disease_assessment)
#'                 )
#' build_ggswim(ggswim_obj)
build_ggswim <- function(ggswim_obj) {
  # Checks ----
  check_ggswim_obj(ggswim_obj)

  ggswim_obj |>
    # remove ggswim class, so default ggplot2 print methods will take over
    structure(class = class(ggswim_obj) |> setdiff("ggswim_obj"))
}
