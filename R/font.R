#' @title Search for FontAwesome aliases to include in ggswim
#' @description
#' Check strings against the available aliases for FontAwesome icons.
#' @param str A character string alias to search the icon data available.
#' If left empty, the default "", returns all available aliases.
#' @param type A character string denoting which FontAwesome library to search.
#' One of "solid", "regular", or "brands". Default "solid".
#' @param approximate Use approximate or exact matching, TRUE/FALSE. Default `FALSE`.
#' @returns Matching aliases from the available FontAwesome data
#' @examples
#' search_fontawesome("fa-car")
#'
#' @export
search_fontawesome <- function(str = "", type = "solid", approximate = FALSE) {
  # Delegate the search to the utility function
  search_aliases(
    str = str,
    type = type,
    approximate = approximate
  )
}

#' @title Utility Function to Search Aliases Across Icon Libraries
#' @description
#' A generic function to search for aliases within specified icon libraries.
#' @param str A character string alias to search the specified icon library.
#' If left empty (default ""), it returns all available aliases.
#' @param type For FontAwesome dataset only. Specifies the subset to search within.
#' One of "solid", "regular", or "brands". Ignored for other datasets. Default is "solid".
#' @param approximate Logical. If `TRUE`, performs approximate matching using `agrep`.
#' If `FALSE` (default), performs exact matching using `grep`.
#' @returns A sorted character vector of matching aliases.
#' @keywords internal
search_aliases <- function(str = "",
                           type = "solid",
                           approximate = FALSE) {
  # Determine the specific FontAwesome dataframe based on 'type'
  fa_df <- switch(type,
    "regular" = "fa-regular-400",
    "brands" = "fa-brands-400",
    "solid" = "fa-solid-900",
    # Fallback to "fa-solid-900" if 'type' is unrecognized
    "fa-solid-900"
  )

  # Retrieve the aliases vector from the selected FontAwesome dataframe
  aliases <- FontAwesome[[fa_df]][["aliases"]]

  # Perform the search based on the 'approximate' flag
  if (approximate) {
    # Approximate matching using agrep
    hits <- agrep(str, aliases, value = FALSE, ignore.case = TRUE)
  } else {
    # Exact matching using grep
    hits <- grep(str, aliases, ignore.case = TRUE)
  }

  # Extract the matching aliases
  out <- aliases[hits]

  # If no matches found, return an empty character vector
  if (length(out) == 0) {
    return(character(0))
  }

  # Sort and return the matching aliases
  sort(out)
}

#' @title Retrieve FontAwesome Unicode
#' @description
#' Convert FontAwesome alias strings to their corresponding Unicode format.
#'
#' All `aliases` should be prepended with "fa".
#'
#' @details
#' When using [fontawesome] outputs with [geom_swim_marker], the following options
#' are available for the `family` argument:
#'
#' - "FontAwesome-Brands"
#' - "FontAwesome-Regular"
#' - "FontAwesome-Solid"
#'
#' @param aliases A string or vector of strings to retrieve Unicode values for.
#' @param type A character string denoting which FontAwesome library subset to search.
#' One of "solid", "regular", or "brands". Default is "solid".
#' @returns A named character vector of Unicode values corresponding to the provided aliases.
#' If an alias is not found, its value will be `NA`.
#' @examples
#' fontawesome(c("fa-car", "fa-user"))
#' @export
fontawesome <- function(aliases, type = "solid") {
  # Ensure aliases is a character vector
  aliases <- as.character(aliases)

  # Retrieve Unicode using the utility function
  unicode <- retrieve_unicode(
    aliases = aliases,
    type = type
  )

  return(unicode)
}

#' @title Retrieve Unicode for Icon Aliases Across Libraries
#' @description
#' A generic function to retrieve Unicode values for given icon aliases from specified icon libraries.
#' This function reduces code duplication by handling the core retrieval logic.
#' @param aliases A character vector of aliases to retrieve Unicode values for.
#' @param type For FontAwesome dataset only. Specifies the subset to search within.
#' One of "solid", "regular", or "brands". Ignored for other datasets. Default is "solid".
#' @returns A named character vector of Unicode values corresponding to the provided aliases.
#' If an alias is not found, its value will be `NA`.
#' @examples
#' # Retrieve Unicode for FontAwesome solid icons
#' retrieve_unicode(c("fa-car", "fa-user"), type = "solid")
#'
#' @noRd
retrieve_unicode <- function(aliases,
                             type = "solid") {
  # Map the 'type' to the corresponding FontAwesome dataframe
  fa_df <- switch(type,
    "regular" = "fa-regular-400",
    "brands"  = "fa-brands-400",
    "solid"   = "fa-solid-900",
    "fa-solid-900"
  ) # Default fallback

  # Check if the dataframe exists within FontAwesome
  if (!fa_df %in% names(FontAwesome)) {
    stop(sprintf("FontAwesome dataframe '%s' does not exist.", fa_df))
  }

  # Access the FontAwesome dataframe
  df_data <- FontAwesome[[fa_df]]

  # Ensure the dataframe has the necessary columns
  if (!all(c("aliases", "fa") %in% colnames(df_data))) {
    stop(sprintf(
      "Dataframe '%s' must contain 'aliases' and 'fa' columns.",
      fa_df
    ))
  }

  # Create a named vector for efficient lookup: names are aliases, values are Unicode
  lookup <- setNames(df_data[["fa"]], df_data[["aliases"]])

  # Initialize the Unicode vector with NA for all aliases
  unicode <- setNames(rep(NA_character_, length(aliases)), aliases)

  # Identify which aliases are present in the lookup
  matched <- aliases %in% names(lookup)

  # Assign the corresponding Unicode values to matched aliases
  unicode[matched] <- lookup[aliases[matched]]

  # Determine which aliases were not found
  invalid_aliases <- aliases[!matched]

  # Notify the user about invalid aliases, if any
  if (length(invalid_aliases) > 0) {
    message("Invalid: ", paste(invalid_aliases, collapse = ", "))
  }

  return(unicode)
}

#' @title Load Select Fonts
#'
#' @description
#' Load open source fonts from the web into the environment for use with ggswim
#' functions.
#'
#' @details
#' Currently the following fonts are supported:
#'
#' - FontAwesome
#'    - Brands
#'    - Regular
#'    -Solid
#'
#' @param verbose Whether or not to display load output messages. Default `TRUE`.
#' @export

load_fonts <- function(verbose = TRUE) {
  # mapping for naming consistency
  custom_names <- c(
    "fa-brands-400"   = "FontAwesome-Brands",
    "fa-regular-400"  = "FontAwesome-Regular",
    "fa-solid-900"    = "FontAwesome-Solid"
  )

  # List the font families to load.
  # For FontAwesome fonts, these will be fetched remotely.
  pkg_fonts <- c(
    "fa-brands-400",
    "fa-regular-400",
    "fa-solid-900"
  )

  .load_pkg_font <- function(family) {
    # Prepare the font features as before
    features <- list("kern" = 1, "zero" = 0)
    feature_spec <- do.call(font_feature, features)

    if (family %in% c("fa-brands-400", "fa-regular-400", "fa-solid-900")) {
      # Construct the remote URL using the "raw" GitHub URL
      font_url <- sprintf("https://github.com/FortAwesome/Font-Awesome/raw/6.x/webfonts/%s.ttf", family)
      # Download the font to a temporary file
      temp_file <- tempfile(fileext = ".ttf")
      tryCatch(
        {
          download.file(font_url, destfile = temp_file, mode = "wb", quiet = TRUE)
        },
        error = function(e) {
          warning(sprintf("Failed to download font '%s' from %s", family, font_url))
          return(invisible(NULL))
        }
      )
      # Use the custom name if provided
      font_name <- if (family %in% names(custom_names)) custom_names[family] else family
      register_font(name = font_name, plain = temp_file, features = feature_spec)
      if (verbose) {
        cli({
          cli_h2("{.strong {family}}")
          cli_alert_success("1 style registered: {.val {font_name}}")
        })
      }
    } else {
      # Load from local package directory for non-remote fonts
      font_dir <- system.file("fonts", family, package = "ggswim")
      font_paths <- dir(font_dir, full.names = TRUE)
      font_names <- sub("\\..*$", "", dir(font_dir))
      if (all(font_names %in% names(custom_names))) {
        font_names <- unname(custom_names[font_names])
      }
      walk2(font_names, font_paths, function(name, path) {
        register_font(name = name, plain = path, features = feature_spec)
      })
      if (verbose) {
        cli({
          cli_h2("{.strong {family}}")
          cli_alert_success("{.val {length(font_names)}} style{?s} registered:")
          cli_ul(font_names)
        })
      }
    }
  }

  # Iterate over the specified font families
  walk(pkg_fonts, .load_pkg_font)

  if (verbose) {
    cli_rule()
    cli_alert_info("Done! Check {.code registry_fonts()} for more details.")
  }

  invisible(registry_fonts())
}
