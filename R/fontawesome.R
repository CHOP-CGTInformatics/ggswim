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
search_fontawesome <- function(str = "", approximate=FALSE) {
  fa_env$search(str=str, type='aliases', approximate=approximate, font_data=fa)
}

#' @title Load fontawesome fonts for use with ggswim
#' @description
#' ggswim comes installed with fontawesome free solid fonts. By default, [load.fa()]
#' is enabled when ggswim is imported.
#' @param font an internal TTF file
#' @return NULL
#' @examples
#' load.fa()
#'
#' @export
load.fa <- function(font = "fa.ttf") {
  fa_env$load_font(font=font)
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
  res <- fa_env$toUnicode(aliases=aliases, font_data=fa)
  ii <- is.na(res)
  if (any(ii)) {
    message('Invalid: ', paste(aliases[ii], collapse=', '))
  }
  return(res)
}

#' @noRd
#' @keywords internal
fa_env <- proto(expr = {
  get_path <- function(.) {
    system.file("fontawesome/solid", package="ggswim")
  }
  load_font <- function(., font) {
    wd <- getwd()
    font_path <- get_path()
    setwd(font_path)
    if (!file.exists(font)) {
      setwd(wd)
      stop("font doesn't exist...")
    }

    font_add(sub("\\..*", "", font), font)
    showtext_auto()
    setwd(wd)
  }
  search <- function(., str, type, approximate=FALSE, font_data=fa) {
    if (approximate) {
      i <- agrep(str, font_data[[type]])
    } else {
      i <- grep(str, font_data[[type]])
    }
    unlist(font_data$aliases[i])
  }
  toUnicode <- function(., aliases, font_data=font_data) {
    ii <- sapply(aliases, function(alias) {
      i <- which(sapply(font_data$aliases, function(x) alias %in% x))
      if (length(i) == 0)
        return(NA)
      return(i)
    })
    if (all(is.na(ii)))
      return(NA)
    font_data[ii,1][["fa"]]
  }
})
