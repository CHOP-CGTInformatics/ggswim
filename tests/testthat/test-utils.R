df <- mtcars
df$names <- rownames(mtcars)
rownames(df) <- NULL
df$cyl <- factor(df$cyl)

simple_ggplot <- ggplot() +
  geom_point(df, mapping = aes(x = hp, y = mpg, color = names))

ggswim_plot <- ggswim(df, aes(x = hp, y = mpg, fill = cyl)) +
  add_marker(mapping = aes(x = hp, y = mpg, color = names)) +
  add_marker(df[c("hp", "mpg", "wt")],
    mapping = aes(x = hp, y = mpg, name = "test"),
    color = "firebrick"
  ) |>
    suppressWarnings()

# Static colour df representative of above ggswim_plot
static_colour_df <- data.frame(
  "indices" = 3, "colors" = "firebrick", "name" = "test"
)

test_that("get_layer_data works with simple dataset and aes colour mapping", {
  layer_data <- get_layer_data(
    data = simple_ggplot$layers[[1]]$data,
    mapping = simple_ggplot$layers[[1]]$mapping,
    i = 1L
  )

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
})

test_that("get_layer_data works with swim framework and aes fill mapping", {
  layer_data <- get_layer_data(
    data = df,
    mapping = ggswim_plot$layers[[1]]$mapping,
    i = 1L
  )

  expect_true(all(c("fill_mapping", "fill") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
})


test_that("get_layer_data works with a swim framework and static colour mapping", {
  layer_data <- get_layer_data(
    data = ggswim_plot$layers[[3]]$data,
    mapping = ggswim_plot$layers[[3]]$mapping,
    i = 3L
  )

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
  # Ensure that using static colour results in a single value for colour
  # and colour mapping
  expect_true(length(unique(layer_data$colour)) == 1)
  expect_true(length(unique(layer_data$colour_mapping)) == 1)
})

test_that("get_layer_data works with a swim framework and coerced mapping", {
  ggswim_plot_coercions <- ggswim(df, aes(x = hp, y = mpg, fill = cyl)) +
    add_marker(mapping = aes(x = hp, y = mpg, color = factor(names)))

  expect_no_error(
    get_layer_data(
      data = ggswim_plot_coercions$data,
      mapping = ggswim_plot_coercions$layers[[2]]$mapping,
      i = 2L
    )
  )
})

test_that("retrieve_original_aes works", {
  data <- mtcars
  coerced_mapping <- aes(x = "cyl", y = "hp", color = factor("disp"))
  non_coerced_mapping <- aes(x = "cyl", y = "hp", color = "disp")

  expect_no_error(
    retrieve_original_aes(data = data, aes_mapping = unlist(non_coerced_mapping), aes_var = "colour")
  )
  expect_no_error(
    retrieve_original_aes(data = data, aes_mapping = unlist(coerced_mapping), aes_var = "colour")
  )
  expect_equal(
    retrieve_original_aes(data = data, aes_mapping = unlist(non_coerced_mapping), aes_var = "colour"),
    retrieve_original_aes(data = data, aes_mapping = unlist(coerced_mapping), aes_var = "colour")
  )
})
