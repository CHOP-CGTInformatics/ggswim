df <- mtcars
df$names <- rownames(mtcars)
rownames(df) <- NULL

p1 <- ggplot() +
  geom_point(df, mapping = aes(x = hp, y = mpg, color = names))

p2 <- ggswim(df, aes(x = hp, y = mpg, fill = cyl)) +
  add_marker(mapping = aes(x = hp, y = mpg, color = names)) +
  add_marker(df[c("hp", "mpg", "wt")],
             mapping = aes(x = hp, y = mpg, name = "test"),
             color = "firebrick") |>
  suppressWarnings()

# Static colour df representative of above p2
static_colour_df <- data.frame(
  "indices" = 3, "colors" = "firebrick", "name" = "test"
)

test_that("get_layer_data works with simple dataset and aes mapping", {
  layer_data <- get_layer_data(data = p1$layers[[1]]$data,
                 mapping = p1$layers[[1]]$mapping,
                 i = 1L)

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
})


test_that("get_layer_data works with a swim framework and static mapping", {
  layer_data <- get_layer_data(data = p2$layers[[3]]$data,
                               mapping = p2$layers[[3]]$mapping,
                               i = 3L)

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
  # Ensure that using static colour results in a single value for colour
  # and colour mapping
  expect_true(length(unique(layer_data$colour)) == 1)
  expect_true(length(unique(layer_data$colour_mapping)) == 1)
})
