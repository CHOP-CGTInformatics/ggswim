df <- mtcars
df$names <- rownames(mtcars)
rownames(df) <- NULL
df$cyl <- factor(df$cyl)

# Fixed colour df representative of above ggswim_plot
fixed_colour_df <- data.frame(
  "indices" = 3, "colors" = "firebrick", "name" = "test"
)

test_that("get_layer_data works with simple dataset and aes colour mapping", {
  simple_ggplot <- ggplot() +
    geom_point(df, mapping = aes(x = hp, y = mpg, color = names))

  layer_data <- get_layer_data(
    data = simple_ggplot$layers[[1]]$data,
    mapping = simple_ggplot$layers[[1]]$mapping,
    i = 1L
  )

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
})

test_that("get_layer_data works with swim framework and aes color mapping", {
  ggswim_plot <- ggswim(
    patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  )

  layer_data <- get_layer_data(
    data = patient_data,
    mapping = ggswim_plot$layers[[1]]$mapping,
    i = 1L
  )

  expect_true(all(c("colour_mapping", "colour") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
})


test_that("get_layer_data works with a swim framework and fixed colour mapping", {
  suppressWarnings({
    ggswim_plot <- ggswim(
      patient_data,
      aes(
        x = start_time,
        xend = end_time,
        y = pt_id,
        color = disease_assessment
      )
    ) +
      add_marker(
        data = infusion_events |> mutate(infusion = "Infusion"),
        aes(x = time_from_initial_infusion, y = pt_id, name = "Initial Infusion"),
        color = "red"
      )
  })


  layer_data <- get_layer_data(
    data = ggswim_plot$layers[[2]]$data,
    mapping = ggswim_plot$layers[[2]]$mapping,
    i = 2L
  )

  expect_true(all(c("colour", "colour_mapping") %in% names(layer_data)))
  expect_equal(class(layer_data), "data.frame")
  # Ensure that using fixed colour results in a single value for colour
  # and colour mapping
  expect_true(length(unique(layer_data$colour)) == 1)
  expect_true(length(unique(layer_data$colour_mapping)) == 1)
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

  expect_true(is.character(retrieve_original_aes(
    data = data,
    aes_mapping = unlist(coerced_mapping),
    aes_var = "colour"
  )))
  expect_true(length(retrieve_original_aes(
    data = data,
    aes_mapping = unlist(coerced_mapping),
    aes_var = "colour"
  )) == 1)
})
