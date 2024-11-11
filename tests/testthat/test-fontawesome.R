test_that("load.fa works", {
  expect_no_condition(load.fa())
})

test_that("search_fontawesome works", {
  all_vals <- search_fontawesome()
  specific_vals <- search_fontawesome(str = "fa-car")

  expect_equal(length(all_vals), nrow(fa))
  expect_true(all(stringr::str_detect(specific_vals, "car")))
})

test_that("fontawesome works", {
  out <- fontawesome("fa-dog")
  expect_equal(class(out), "character")
  expect_true(length(out) == 1)

  expect_message(fontawesome("notarealicon"), "Invalid: notarealicon")
  expect_true(is.na(fontawesome("notarealicon")) |> suppressMessages())
})

test_that("fa_env works", {
  path <- fa_env$get_path()
  expect_equal(class(fa_env$get_path()), "character")
  expect_true(nchar(fa_env$get_path()) > 0)

  expect_no_condition(fa_env$load_font(font = "fa.ttf"))
  expect_error(fa_env$load_font(font = "fake_path"))

  aliases <- fa_env$search(str = "fa-car", type = "aliases")
  expect_equal(class(aliases), "character")
  expect_true(length(aliases) > 0)
  approx_aliases <- fa_env$search(str = "fa-car", type = "aliases", approximate = TRUE)
  expect_equal(class(aliases), "character")
  expect_true(length(aliases) > 0)
  expect_true(length(approx_aliases) > length(aliases))


  unicode <- fa_env$toUnicode(aliases = "fa-car", font_data = fa)
  expect_equal(class(unicode), "character")
  expect_true(length(unicode) > 0)
})
