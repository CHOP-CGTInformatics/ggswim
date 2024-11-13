# This file kept as a reference for unpacking font files (TTFs) and converting
# them into usable styles

# Force reticulate to look for virtual env. Python executable
reticulate::use_virtualenv("./.venv", required = TRUE)
# Source and create RDA files
reticulate::source_python("data-raw/fonttools.py")

load("inst/fonts/FontAwesome/fa-solid-900.rda")
file.remove("inst/fonts/FontAwesome/fa-solid-900.rda")
load("inst/fonts/FontAwesome/fa-regular-400.rda")
file.remove("inst/fonts/FontAwesome/fa-regular-400.rda")
load("inst/fonts/FontAwesome/fa-brands-400.rda")
file.remove("inst/fonts/FontAwesome/fa-brands-400.rda")


# Create the FontAwesome rds for sysdata.rda
FontAwesome <- list("fa-solid-900" = `fa-solid-900`,
                    "fa-regular-400" = `fa-regular-400`,
                    "fa-brands-400" = `fa-brands-400`)

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

usethis::use_data(FontAwesome, internal = TRUE, overwrite = TRUE)
