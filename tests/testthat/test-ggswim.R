test_that("ggswim works", {
  # Test suite outdated. To be updated after WIP completed
  # df <- tibble::tribble(
  #   ~subject_id, ~years, ~indicator_1, ~indicator_2, ~status,
  #   1, 2000, 1, 1, "unknown",
  #   1, 2001, 0, 0, "unknown",
  #   1, 2002, 1, 0, "positive",
  #   1, 2003, 0, 1, "negative",
  #   2, 2000, 0, 0, "positive",
  #   2, 2001, 0, 0, "negative",
  #   2, 2002, 0, 1, "negative",
  #   3, 2000, 0, 1, "negative",
  #   4, 2000, 0, 0, "negative",
  #   4, 2001, 1, 0, "positive",
  #   4, 2002, 0, 1, "positive"
  # )
  #
  # df_swim <- df |>
  #   streamline(id = subject_id,
  #              time = years,
  #              markers = c(indicator_1, indicator_2),
  #              lanes = status)
  #
  # out <- ggswim(df_swim)
  #
  # expect_true(all(c("gg", "ggplot") %in% class(out)))
  # # Expect 3 layers: 1 line, 2 points
  # expect_equal(length(out$layers), 3)
})
