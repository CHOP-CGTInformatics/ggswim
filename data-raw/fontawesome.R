# This file used internally to convert TTF files from FontAwesome into usable
# sysdata

# Force reticulate to look for virtual env. Python executable
reticulate::use_virtualenv("./.venv", required = TRUE)
# Source and create RDA files
reticulate::source_python("data-raw/fonttools.py")

paths <- c("inst/fonts/FontAwesome", "inst/fonts/Bootstrap")
rda_files <- list.files(paths, pattern = "\\.rda$", full.names = TRUE)
purrr::walk(rda_files, ~load(.x, envir = .GlobalEnv))
purrr::walk(rda_files, ~file.remove(.x))

# Create the FontAwesome rds for sysdata.rda
FontAwesome <- list("fa-solid-900" = `fa-solid-900`,
                    "fa-regular-400" = `fa-regular-400`,
                    "fa-brands-400" = `fa-brands-400`)

# Create Bootstrap rds for sysdata.rda
Bootstrap <- list("bootstrap-icons" = `bootstrap-icons`)

# Prepend "fa-" to each alias when multiple aliases are present
FontAwesome <- lapply(FontAwesome, function(df) {
  df$aliases <- sapply(strsplit(df$aliases, ","), function(alias_vector) {
    # Trim whitespace and prepend "fa-" to each alias
    modified_aliases <- paste0("fa-", trimws(alias_vector))
    # Recombine the aliases into a single string
    paste(modified_aliases, collapse = ", ")
  })
  tibble::tibble(df)
})

# Prepend "bs-" to each alias when multiple aliases are present
Bootstrap <- lapply(Bootstrap, function(df) {
  df$aliases <- sapply(strsplit(df$aliases, ","), function(alias_vector) {
    # Trim whitespace and prepend "fa-" to each alias
    modified_aliases <- paste0("bs-", trimws(alias_vector))
    # Recombine the aliases into a single string
    paste(modified_aliases, collapse = ", ")
  })
  tibble::tibble(df)
})

usethis::use_data(FontAwesome, Bootstrap, internal = TRUE, overwrite = TRUE)
