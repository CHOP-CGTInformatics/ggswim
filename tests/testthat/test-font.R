test_that("load_fonts works", {
  expect_no_condition(load_fonts(verbose = FALSE))
})

test_that("invalid fonts produce warning and return invisible NULL", {
  expect_warning(
    res <- load_fonts(fonts = "invalid-font", verbose = FALSE),
    "No valid font families specified. Nothing to load."
  )
  expect_null(res)
})

test_that("valid fonts load and registry contains expected fonts", {
  # Use a subset of valid fonts
  res <- load_fonts(fonts = c("bootstrap-icons", "fa-solid-900"), verbose = FALSE)
  # Retrieve the registry of loaded fonts
  reg <- registry_fonts()

  # Check that the registry is a tibble (data frame) and contains the expected font families.
  expect_s3_class(reg, "tbl_df")
  expect_true(any(grepl("Bootstrap", reg$family)))
  expect_true(any(grepl("FontAwesome-Solid", reg$family)))
})

test_that("download error produces warning", {
  # Determine the cache directory and the expected cached file path.
  cache_dir <- tools::R_user_dir("ggswim", which = "cache")
  cached_file <- file.path(cache_dir, "fa-brands-400.ttf")

  # Remove the cached file if it exists so that the download is forced.
  if (file.exists(cached_file)) {
    unlink(cached_file)
  }

  # Use local_mocked_bindings to override download.file within the scope of this test.
  local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      stop("Simulated download failure")
    },
    .env = baseenv()
  )

  expect_warning(
    load_fonts(fonts = "fa-brands-400", verbose = FALSE),
    regexp = "Failed to download font 'fa-brands-400' from"
  )
})

test_that("search_fontawesome works", {
  all_vals <- search_fontawesome()
  specific_vals <- search_fontawesome(str = "fa-car")

  expect_equal(length(all_vals), nrow(FontAwesome[["fa-solid-900"]]))
  expect_true(all(stringr::str_detect(specific_vals, "car")))
})

test_that("fontawesome works", {
  out <- fontawesome("fa-dog")
  expect_equal(class(out), "character")
  expect_true(length(out) == 1)

  expect_message(fontawesome("notarealicon"), "Invalid: notarealicon")
  expect_true(is.na(fontawesome("notarealicon")) |> suppressMessages())
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
  expect_setequal(search_aliases("dog", dataset = "FontAwesome"),
                  c("fa-dog", "fa-hotdog", "fa-shield-dog"))

  expect_true(length(search_aliases("dove", dataset = "FontAwesome", approximate = FALSE)) == 1)
  expect_true(length(search_aliases("dove", dataset = "FontAwesome", approximate = TRUE)) > 1)

  expect_equal(search_aliases("cookie", dataset = "Bootstrap"),
                  c("bs-cookie"))

  expect_equal(search_aliases("dog", dataset = "Bootstrap"),
               character(0))

  expect_error(search_aliases(dataset = "Test"))
})
