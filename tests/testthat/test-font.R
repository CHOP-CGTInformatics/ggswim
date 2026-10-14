# Keep font-loading tests independent of the network and user registry/cache.
local_font_cache <- function(.local_envir = parent.frame()) {
  fonts <- systemfonts::system_fonts()
  skip_if(nrow(fonts) == 0, "No system font available for cache validation")
  source <- fonts$path[[1]]
  skip_if_not(valid_font_file(source), "No readable test font")
  cache_root <- tempfile("ggswim-test-fonts-")
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root), .local_envir = .local_envir)
  cache <- tools::R_user_dir("ggswim", which = "cache")
  dir.create(cache, recursive = TRUE)
  withr::defer(unlink(cache_root, recursive = TRUE), envir = .local_envir)
  state <- new.env(parent = emptyenv())
  state$cache <- cache
  state$source <- source
  state$downloads <- 0L
  state$registered <- character()
  local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      state$downloads <- state$downloads + 1L
      file.copy(source, destfile, overwrite = TRUE)
      0L
    },
    register_font = function(name, plain, features) {
      stopifnot(valid_font_file(plain))
      state$registered <- c(state$registered, name)
    },
    registry_fonts = function() tibble::tibble(family = state$registered),
    .package = "ggswim",
    .env = .local_envir
  )
  state
}

test_that("fonts are downloaded once and reused offline", {
  state <- local_font_cache()
  result <- withVisible(load_fonts(fonts = "fa-solid-900", verbose = FALSE))
  expect_false(result$visible)
  expect_equal(result$value$family, "FontAwesome-Solid")
  expect_equal(state$downloads, 1L)
  local_mocked_bindings(download.file = function(...) stop("Offline"))
  expect_no_condition(load_fonts(fonts = "fa-solid-900", verbose = FALSE))
  expect_equal(state$registered, rep("FontAwesome-Solid", 2))
  expect_equal(list.files(state$cache), "fa-solid-900.ttf")
})

test_that("default font selection preserves family names", {
  state <- local_font_cache()
  expect_no_condition(load_fonts(verbose = FALSE))
  expect_equal(state$downloads, 4L)
  expect_equal(state$registered, c(
    "Bootstrap", "FontAwesome-Brands", "FontAwesome-Regular", "FontAwesome-Solid"
  ))
})

test_that("invalid fonts produce warning and return invisible NULL", {
  expect_warning(
    result <- withVisible(load_fonts(fonts = "invalid-font", verbose = FALSE)),
    "No valid font families specified. Nothing to load."
  )
  expect_null(result$value)
  expect_false(result$visible)
})

test_that("failed partial downloads do not enter the cache", {
  state <- local_font_cache()
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("partial download", destfile)
    stop("Offline")
  })
  expect_warning(
    load_fonts(fonts = "fa-brands-400", verbose = FALSE),
    "Failed to download font 'fa-brands-400'.*Offline"
  )
  expect_length(state$registered, 0)
  expect_length(list.files(state$cache), 0)
})

test_that("nonzero download status and unreadable fonts are rejected", {
  state <- local_font_cache()
  local_mocked_bindings(download.file = function(...) 1L)
  expect_warning(
    load_fonts(fonts = "fa-solid-900", verbose = FALSE), "nonzero status"
  )
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("not a font", destfile)
    0L
  })
  expect_warning(
    load_fonts(fonts = "fa-solid-900", verbose = FALSE), "not a readable font"
  )
  expect_length(state$registered, 0)
  expect_length(list.files(state$cache), 0)
})

test_that("corrupt cached fonts are replaced only after a valid download", {
  state <- local_font_cache()
  cached <- file.path(state$cache, "fa-solid-900.ttf")
  writeLines("corrupt", cached)
  expect_no_condition(load_fonts(fonts = "fa-solid-900", verbose = FALSE))
  expect_true(valid_font_file(cached))
  expect_equal(state$downloads, 1L)
})

test_that("registration failures warn and preserve the cached font", {
  state <- local_font_cache()
  local_mocked_bindings(register_font = function(...) stop("Registry failure"))
  expect_warning(
    load_fonts(fonts = "fa-solid-900", verbose = FALSE),
    "Failed to register font 'fa-solid-900': Registry failure"
  )
  expect_true(valid_font_file(file.path(state$cache, "fa-solid-900.ttf")))
})

test_that("search_fontawesome works", {
  all_vals <- search_fontawesome()
  specific_vals <- search_fontawesome(str = "fa-car")

  expect_equal(length(all_vals), nrow(FontAwesome[["fa-solid-900"]]))
  expect_true(all(stringr::str_detect(specific_vals, "car")))
})

test_that("search_fontawesome validates type", {
  expect_error(
    search_fontawesome(type = "invalid"),
    "must be one of"
  )
})

test_that("fontawesome works", {
  out <- fontawesome("fa-dog")
  expect_equal(class(out), "character")
  expect_true(length(out) == 1)

  expect_message(fontawesome("notarealicon"), "Invalid: notarealicon")
  expect_true(is.na(fontawesome("notarealicon")) |> suppressMessages())
})

test_that("fontawesome validates type", {
  expect_error(
    fontawesome("fa-car", type = "invalid"),
    "must be one of"
  )
})


test_that("search_bootstrap works", {
  all_vals <- search_bootstrap()
  specific_vals <- search_bootstrap(str = "bs-car")

  expect_equal(length(all_vals), nrow(Bootstrap[["bootstrap-icons"]]))
  expect_true(all(stringr::str_detect(specific_vals, "car")))
})

test_that("bootstrap works", {
  out <- bootstrap("bs-android")
  expect_equal(class(out), "character")
  expect_true(length(out) == 1)

  expect_message(bootstrap("notarealicon"), "Invalid: notarealicon")
  expect_true(is.na(bootstrap("notarealicon")) |> suppressMessages())
})

test_that("search_aliases works", {
  expect_setequal(
    search_aliases("dog", dataset = "FontAwesome"),
    c("fa-dog", "fa-hotdog", "fa-shield-dog")
  )

  expect_true(
    length(search_aliases(
      "dove",
      dataset = "FontAwesome",
      approximate = FALSE
    )) ==
      1
  )
  expect_true(
    length(search_aliases(
      "dove",
      dataset = "FontAwesome",
      approximate = TRUE
    )) >
      1
  )

  expect_equal(
    search_aliases("cookie", dataset = "Bootstrap"),
    c("bs-cookie")
  )

  expect_equal(
    search_aliases("dog", dataset = "Bootstrap"),
    character(0)
  )

  expect_error(search_aliases(dataset = "Test"))
})
