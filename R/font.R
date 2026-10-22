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

#' @noRd
get_fontawesome_dataset <- function(type) {
  # FontAwesome ships separate files for each supported icon family.
  # Validate first so unsupported values cannot silently fall back to solid.
  type <- rlang::arg_match(type, c("solid", "regular", "brands"))

  switch(
    type,
    "regular" = "fa-regular-400",
    "brands" = "fa-brands-400",
    "solid" = "fa-solid-900"
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
search_aliases <- function(
  str = "",
  dataset = c("FontAwesome", "Bootstrap"),
  type = "solid",
  approximate = FALSE
) {
  # Ensure the dataset argument is matched correctly
  dataset <- match.arg(dataset)

  # Select the appropriate aliases vector based on dataset and type
  if (dataset == "FontAwesome") {
    # FontAwesome search is limited to the selected icon family.
    fa_df <- get_fontawesome_dataset(type)
    aliases <- FontAwesome[[fa_df]][["aliases"]]
  } else if (dataset == "Bootstrap") {
    # Bootstrap has one icon family, so the FontAwesome type is ignored.
    bs_df <- "bootstrap-icons"
    aliases <- Bootstrap[[bs_df]][["aliases"]]
  } else {
    cli_abort(
      "Unsupported dataset {.val {dataset}}. Choose either {.val FontAwesome} or {.val Bootstrap}."
    )
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

  unicode
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

  unicode
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
retrieve_unicode <- function(
  aliases,
  dataset = c("FontAwesome", "Bootstrap"),
  type = "solid"
) {
  # Ensure the dataset argument is matched correctly
  dataset <- match.arg(dataset)

  # Determine the dataframe based on dataset and type
  if (dataset == "FontAwesome") {
    # FontAwesome unicode lookup depends on the selected icon family.
    fa_df <- get_fontawesome_dataset(type)

    # Check if the dataframe exists within FontAwesome
    if (!fa_df %in% names(FontAwesome)) {
      cli::cli_abort("FontAwesome dataset {.val {fa_df}} does not exist.")
    }

    # Access the FontAwesome dataframe
    df_data <- FontAwesome[[fa_df]]
  } else if (dataset == "Bootstrap") {
    # Bootstrap has one icon family, so the FontAwesome type is ignored.
    bs_df <- "bootstrap-icons"

    # Check if the dataframe exists within Bootstrap
    if (!bs_df %in% names(Bootstrap)) {
      cli::cli_abort("Bootstrap dataset {.val {bs_df}} does not exist.")
    }

    # Access the Bootstrap dataframe
    df_data <- Bootstrap[[bs_df]]
  } else {
    cli::cli_abort(
      "Unsupported dataset {.val {dataset}}. Choose either {.val FontAwesome} or {.val Bootstrap}."
    )
  }

  # Ensure the dataframe has the necessary columns
  if (!all(c("aliases", "fa") %in% colnames(df_data))) {
    cli::cli_abort(
      "Dataset {.val {dataset_name}} must contain columns {.field aliases} and {.field fa}.",
      dataset_name = ifelse(dataset == "FontAwesome", fa_df, bs_df)
    )
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

  unicode
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
#' The default is to load all supported fonts. Valid cached files are reused,
#' so subsequent calls can work offline. Missing or unreadable fonts are
#' downloaded to a temporary file and validated before entering the cache.
#' Download or registration failures produce a warning and skip that font.
#'
#' @param fonts A character vector of font families to load. Valid options are
#'   `"bootstrap-icons"`, `"fa-brands-400"`, `"fa-regular-400"`, and `"fa-solid-900"`.
#'   The default is to load all supported fonts.
#' @param verbose Whether or not to display load output messages. Default is `TRUE`.
#'
#' @return Invisibly returns the font registry as a tibble.
#'
#' @export
load_fonts <- function(
  fonts = c(
    "bootstrap-icons",
    "fa-brands-400",
    "fa-regular-400",
    "fa-solid-900"
  ),
  verbose = TRUE
) {
  # Mapping for naming consistency
  custom_names <- c(
    "bootstrap-icons" = "Bootstrap",
    "fa-brands-400" = "FontAwesome-Brands",
    "fa-regular-400" = "FontAwesome-Regular",
    "fa-solid-900" = "FontAwesome-Solid"
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
    font_url <- sprintf(
      "https://raw.githubusercontent.com/CHOP-CGTInformatics/ggswim/main/font/fonts/%s.ttf",
      family
    )

    cached_file <- file.path(cache_dir, sprintf("%s.ttf", family))

    if (!valid_font_file(cached_file)) {
      temporary_file <- tempfile(pattern = "ggswim-font-", tmpdir = cache_dir)
      on.exit(unlink(temporary_file), add = TRUE)
      downloaded <- tryCatch(
        {
          status <- download.file(
            font_url,
            destfile = temporary_file,
            mode = "wb",
            quiet = TRUE
          )
          if (!identical(status, 0L) && !identical(status, 0)) {
            stop("Download returned a nonzero status.")
          }
          if (!valid_font_file(temporary_file)) {
            stop("Downloaded file is not a readable font.")
          }
          if (!file.rename(temporary_file, cached_file)) {
            stop("Could not move the downloaded font into the cache.")
          }
          TRUE
        },
        error = function(e) {
          warning(
            sprintf(
              "Failed to download font '%s' from %s: %s",
              family,
              font_url,
              conditionMessage(e)
            ),
            call. = FALSE
          )
          FALSE
        }
      )
      if (!downloaded) {
        return(invisible(NULL))
      }
    }

    font_name <- custom_names[[family]]
    registered <- tryCatch(
      {
        register_font(
          name = font_name,
          plain = cached_file,
          features = feature_spec
        )
        TRUE
      },
      error = function(e) {
        warning(
          sprintf(
            "Failed to register font '%s': %s",
            family,
            conditionMessage(e)
          ),
          call. = FALSE
        )
        FALSE
      }
    )
    if (!registered) {
      return(invisible(NULL))
    }

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

#' @noRd
valid_font_file <- function(path) {
  if (!file.exists(path) || is.na(file.size(path)) || file.size(path) == 0) {
    return(FALSE)
  }
  tryCatch(
    {
      # FreeType can leave systemfonts' native face cache unusable after a failed
      # FT_New_Face call. Validate in a child process so malformed downloads
      # cannot crash later font lookups in this R session.
      callr::r(
        function(font_path) {
          info <- withCallingHandlers(
            systemfonts::font_info(path = font_path),
            warning = function(w) stop(w)
          )
          nrow(info) == 1L && isTRUE(info$n_glyphs[[1]] > 0)
        },
        args = list(path),
        show = FALSE
      )
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}
