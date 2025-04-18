rm(list = ls(all.names = TRUE))
deviceType <- ifelse(R.version$os=="linux-gnu", "X11", "windows")
options(device = deviceType) #https://support.rstudio.org/help/discussions/problems/80-error-in-function-only-one-rstudio-graphics-device-is-permitted

spelling::spell_check_package()
# spelling::update_wordlist()
lintr::lint_package()
urlchecker::url_check(); urlchecker::url_update()

styler::style_pkg()

devtools::document()
devtools::check_man() #Should return NULL

# Run as not CRAN to build full vignettes
# devtools::check() sets NOT_CRAN = "true" by default but must set it manually
# as needed for other devtools functions
withr::with_envvar(
  new = c("NOT_CRAN" = "true"),
  devtools::build_vignettes()
)

checks_to_exclude <- c(
  "covr",
  "cyclocomp",
  "lintr_line_length_linter",
  "rcmdcheck_non_ascii_characters_in_data" # For unicode in data
)
gp <-
  goodpractice::all_checks() |>
  purrr::discard(~(. %in% checks_to_exclude)) |>
  {
    \(checks)
    goodpractice::gp(checks = checks)
  }()
goodpractice::results(gp)
gp

devtools::document()
pkgdown::clean_site()
# Run as not CRAN to build full vignettes
withr::with_envvar(
  new = c("NOT_CRAN" = "true"),
  pkgdown::build_site()
)

devtools::run_examples(); #dev.off() #This overwrites the NAMESPACE file too
# pkgload::load_all()
test_results_checked <- devtools::test()

# devtools::check(force_suggests = FALSE)
devtools::check(cran=TRUE)
# check as CRAN
devtools::check(cran=TRUE, env_vars = c(NOT_CRAN = ""))
devtools::check( # Equivalent of R-hub
  manual    = TRUE,
  remote    = TRUE,
  incoming  = TRUE
)

# Note: Must be off of VPN
# rhub::rhub_check(platforms = c("linux", "windows", "ubuntu-next", "ubuntu-release")) # These will be available in GitHub Actions
# devtools::check_win_devel(email = "richardshanna91@gmail.com") # CRAN submission policies encourage the development version
# Careful, the last question ultimately uploads it to CRAN, where you can't delete/reverse your decision.
# Run as not CRAN to build full vignettes
# withr::with_envvar(
#   new = c("NOT_CRAN" = "true"),
#   devtools::release(check=FALSE)
# )
