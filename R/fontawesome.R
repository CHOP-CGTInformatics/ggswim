#' @title Search for fontawesome aliases to include in ggswim
#' @description
#' Check strings against the available aliases for fontawesome icons.
#' @param str A character string alias to search the fontawesome data available.
#' If left empty, the default, will return all possibilities.
#' @param approximate Use approximate or exact matching, TRUE/FALSE. Default `FALSE`.
#' @returns Matching aliases from the available fontawesome data
#' @examples
#' search_fontawesome("fa-car")
#'
#' @export
search_fontawesome <- function(str = "", approximate = FALSE) {
  if (approximate) {
    hits <- agrep(str, FontAwesome[["aliases"]])
  } else {
    hits <- grep(str, FontAwesome[["aliases"]])
  }
  out <- unlist(FontAwesome[["aliases"]][hits])

  # Sort values to better identify instances of multiple hits
  out |>
    sort()
}

#' @title Retrieve fontawesome unicode
#' @description
#' When assigning fontawesome icons as glyphs, [fontawesome()] should be used to
#' convert the alias string to the appropriate Unicode format.
#'
#' All `aliases` should be prepended with "fa".
#'
#' @param aliases A string or vector of strings to retrieve
#' @returns Unicode text
#' @export
#' @examples
#' fontawesome('fa-car')
fontawesome <- function(aliases) {
  matched_rows <- which(FontAwesome$aliases %in% aliases)

  result <- if (length(matched_rows) > 0) {
    matched_rows
  } else {
    NA
  }

  if (all(is.na(result))) {
    out <- NA
  } else {
    out <- FontAwesome[result, 1][["fa"]]
  }

  result <- is.na(out)

  if (any(result)) {
    message("Invalid: ", paste(aliases[result], collapse = ", "))
  }
  return(out)
}

#' @noRd
#' @keywords internal
.load_fonts <- function(verbose = TRUE) {
  custom_names <- c(
    "fa-solid-900" = "FontAwesome"
  )

  .load_pkg_font <- function(family) {
    font_dir <- system.file("fonts", family, package = "ggswim")
    font_paths <- dir(font_dir, full.names = TRUE)
    font_names <- str_remove(dir(font_dir), "\\..*$")

    if (all(font_names %in% names(custom_names))) {
      font_names <- unname(custom_names[font_names])
    }

    walk2(
      font_names, font_paths,
      function(name, path) {
        features <- list("kern" = 1, "zero" = 0)
        if (str_detect(name, "^Inter-")) {
          features <- c(features, "numbers" = "tabular")
        } else if (str_detect(name, "^Piazzolla-")) {
          features <- c(features, "numbers" = "proportional")
        } else if (str_detect(name, "^AtkinsonHyperlegible-")) {
          features <- c(features, "numbers" = "proportional")
        }
        feature_spec <- do.call(font_feature, features)
        register_font(name = name, plain = path, features = feature_spec)
      }
    )
    if (verbose) {
      cli::cli({
        cli::cli_h2("{.strong {family}}")
        cli::cli_alert_success("{.val {length(font_names)}} style{?s} registered:")
        cli::cli_ul(font_names)
      })
    }
  }

  pkg_fonts <- dir(system.file("fonts", package = "ggswim"))
  walk(pkg_fonts, .load_pkg_font)
  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_info("Done! Check {.code registry_fonts()} for more details.")
  }
  invisible(systemfonts::registry_fonts())
}
