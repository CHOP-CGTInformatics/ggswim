# This file kept as a reference for unpacking font files (TTFs) and converting
# them into usable styles

# Force reticulate to look for virtual env. Python executable
reticulate::use_virtualenv("./.venv", required = TRUE)
# Source and create RDA files
reticulate::source_python("data-raw/fonttools.py")

load("inst/fonts/FontAwesome/fa-solid-900.rda")
file.remove("inst/fonts/FontAwesome/fa-solid-900.rda")

# Create the FontAwesome rds for sysdata.rda
FontAwesome <- tibble::tibble(`fa-solid-900`)
FontAwesome$aliases <- paste0("fa-", FontAwesome$aliases)

usethis::use_data(FontAwesome, internal = TRUE, overwrite = TRUE)
