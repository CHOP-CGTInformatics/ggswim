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
    dataset = "FontAwesome",
    type = type,
    approximate = approximate
  )
}

#' @title Search for Bootstrap aliases to include in ggswim
#' @description
#' Check strings against the available aliases for Bootstrap icons.
#' @inheritParams search_fontawesome
#' @returns Matching aliases from the available Bootstrap data
#' @examples
#' search_bootstrap("bs-car-front")
#'
#' @export
search_bootstrap <- function(str = "", approximate = FALSE) {
  # Delegate the search to the utility function
  search_aliases(
    str = str,
    dataset = "Bootstrap",
    type = NULL, # 'type' is irrelevant for Bootstrap
    approximate = approximate
  )
}

#' @title Utility Function to Search Aliases Across Icon Libraries
#' @description
#' A generic function to search for aliases within specified icon libraries.
#' @param str A character string alias to search the specified icon library.
#' If left empty (default ""), it returns all available aliases.
#' @param dataset A character string specifying the icon library to search.
#' Supported values are "FontAwesome" and "Bootstrap". Default is "FontAwesome".
#' @param type For FontAwesome dataset only. Specifies the subset to search within.
#' One of "solid", "regular", or "brands". Ignored for other datasets. Default is "solid".
#' @param approximate Logical. If `TRUE`, performs approximate matching using `agrep`.
#' If `FALSE` (default), performs exact matching using `grep`.
#' @returns A sorted character vector of matching aliases.
#' @keywords internal
search_aliases <- function(str = "",
                           dataset = c("FontAwesome", "Bootstrap"),
                           type = "solid",
                           approximate = FALSE) {
  # Ensure the dataset argument is matched correctly
  dataset <- match.arg(dataset)

  # Select the appropriate aliases vector based on dataset and type
  if (dataset == "FontAwesome") {
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
  } else if (dataset == "Bootstrap") {
    # For Bootstrap, there's no 'type' differentiation
    bs_df <- "bootstrap-icons"

    # Retrieve the aliases vector from the Bootstrap dataframe
    aliases <- Bootstrap[[bs_df]][["aliases"]]
  } else {
    stop("Unsupported dataset. Choose either 'FontAwesome' or 'Bootstrap'.")
  }

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
    dataset = "FontAwesome",
    type = type
  )

  return(unicode)
}

#' @title Retrieve Bootstrap Unicode
#' @description
#' Convert Bootstrap alias strings to their corresponding Unicode format.
#'
#' All `aliases` should be prepended with "bs".
#'
#' @details
#' When using [bootstrap] outputs with [geom_swim_marker], the following options
#' are available for the `family` argument:
#'
#' - "Bootstrap"
#'
#' @inheritParams fontawesome
#' @returns A named character vector of Unicode values corresponding to the provided aliases.
#' If an alias is not found, its value will be `NA`.
#' @examples
#' bootstrap(c("bs-car-front", "bs-heart"))
#' @export
bootstrap <- function(aliases) {
  # Ensure aliases is a character vector
  aliases <- as.character(aliases)

  # Retrieve Unicode using the utility function
  unicode <- retrieve_unicode(
    aliases = aliases,
    dataset = "Bootstrap"
  )

  return(unicode)
}

#' @title Retrieve Unicode for Icon Aliases Across Libraries
#' @description
#' A generic function to retrieve Unicode values for given icon aliases from specified icon libraries.
#' This function reduces code duplication by handling the core retrieval logic.
#' @param aliases A character vector of aliases to retrieve Unicode values for.
#' @param dataset A character string specifying the icon library to search.
#' Supported values are "FontAwesome" and "Bootstrap". Default is "FontAwesome".
#' @param type For FontAwesome dataset only. Specifies the subset to search within.
#' One of "solid", "regular", or "brands". Ignored for other datasets. Default is "solid".
#' @returns A named character vector of Unicode values corresponding to the provided aliases.
#' If an alias is not found, its value will be `NA`.
#' @examples
#' # Retrieve Unicode for FontAwesome solid icons
#' retrieve_unicode(c("fa-car", "fa-user"), dataset = "FontAwesome", type = "solid")
#'
#' # Retrieve Unicode for Bootstrap icons
#' retrieve_unicode(c("bs-car-front", "bs-heart"), dataset = "Bootstrap")
#'
#' @noRd
retrieve_unicode <- function(aliases,
                             dataset = c("FontAwesome", "Bootstrap"),
                             type = "solid") {
  # Ensure the dataset argument is matched correctly
  dataset <- match.arg(dataset)

  # Determine the dataframe based on dataset and type
  if (dataset == "FontAwesome") {
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
  } else if (dataset == "Bootstrap") {
    # Bootstrap does not have different types
    bs_df <- "bootstrap-icons"

    # Check if the dataframe exists within Bootstrap
    if (!bs_df %in% names(Bootstrap)) {
      stop(sprintf("Bootstrap dataframe '%s' does not exist.", bs_df))
    }

    # Access the Bootstrap dataframe
    df_data <- Bootstrap[[bs_df]]
  } else {
    stop("Unsupported dataset. Choose either 'FontAwesome' or 'Bootstrap'.")
  }

  # Ensure the dataframe has the necessary columns
  if (!all(c("aliases", "fa") %in% colnames(df_data))) {
    stop(sprintf(
      "Dataframe '%s' must contain 'aliases' and 'fa' columns.",
      ifelse(dataset == "FontAwesome", fa_df, bs_df)
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
#' Load open source fonts from ggswim's GitHub repository to the user's cache
#' directory.
#'
#' Font files and their associated licenses are available on the main branch.
#'
#' @details
#' Currently the following fonts are supported:
#'
#' - FontAwesome
#'    - Brands
#'    - Regular
#'    - Solid
#' - Bootstrap
#'
#' Users can choose which fonts to load by providing a vector of font family names.
#' The default is to load all supported fonts.
#'
#' @param fonts A character vector of font families to load. Valid options are
#'   `"bootstrap-icons"`, `"fa-brands-400"`, `"fa-regular-400"`, and `"fa-solid-900"`.
#'   The default is to load all supported fonts.
#' @param verbose Whether or not to display load output messages. Default is `TRUE`.
#'
#' @return Invisibly returns the font registry as a tibble.
#'
#' @export
load_fonts <- function(fonts = c("bootstrap-icons", "fa-brands-400", "fa-regular-400", "fa-solid-900"), verbose = TRUE) {
  # Mapping for naming consistency
  custom_names <- c(
    "bootstrap-icons" = "Bootstrap",
    "fa-brands-400"   = "FontAwesome-Brands",
    "fa-regular-400"  = "FontAwesome-Regular",
    "fa-solid-900"    = "FontAwesome-Solid"
  )

  # Use the provided fonts argument; ensure it's a subset of available options.
  pkg_fonts <- intersect(fonts, names(custom_names))
  if (length(pkg_fonts) == 0) {
    warning("No valid font families specified. Nothing to load.")
    return(invisible(NULL))
  }

  # Create a persistent cache directory to store downloaded fonts
  cache_dir <- tools::R_user_dir("ggswim", which = "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  .load_pkg_font <- function(family) {
    features <- list("kern" = 1, "zero" = 0)
    feature_spec <- do.call(font_feature, features)

    # Build raw URL based on the font family.
    font_url <- sprintf("https://raw.githubusercontent.com/CHOP-CGTInformatics/ggswim/main/font/fonts/%s.ttf", family)

    if (is.na(font_url)) {
      warning(sprintf("No remote URL defined for font family '%s'", family))
      return(invisible(NULL))
    }

    # Determine a persistent file path in the cache
    cached_file <- file.path(cache_dir, sprintf("%s.ttf", family))

    # Attempt to download the font file; if download fails, mark download_success as FALSE.
    download_success <- tryCatch({
      download.file(font_url, destfile = cached_file, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning(sprintf("Failed to download font '%s' from %s", family, font_url))
      FALSE
    })

    # If download failed or the file doesn't exist, skip registration.
    if (!download_success || !file.exists(cached_file)) {
      return(invisible(NULL))
    }

    # Use custom naming if available.
    font_name <- if (family %in% names(custom_names)) custom_names[family] else family
    register_font(name = font_name, plain = cached_file, features = feature_spec)

    if (verbose) {
      cli({
        cli_h2("{.strong {family}}")
        cli_alert_success("1 style registered: {.val {font_name}}")
      })
    }
  }

  # Iterate over the specified font families.
  walk(pkg_fonts, .load_pkg_font)

  if (verbose) {
    cli_rule()
    cli_alert_info("Done! Check {.code registry_fonts()} for more details.")
  }

  invisible(registry_fonts())
}

