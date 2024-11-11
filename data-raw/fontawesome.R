# Force reticulate to look for virtual env. Python executable
reticulate::use_virtualenv('./.venv', required=TRUE)
# Source and create RDA files
reticulate::source_python("data-raw/fonttools.py")

load("inst/fontawesome/solid/fa.rda")
file.remove("inst/fontawesome/solid/fa.rda")

fa <- tibble::tibble(fa.solid)
fa$aliases <- paste0("fa-", fa$aliases)

usethis::use_data(fa, internal = TRUE, overwrite = TRUE)
