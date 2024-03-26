pt_data <- tibble::tribble(
  ~"id", ~"trt", ~"end_time", ~"time",
  1, "Drug A", 5, 0,
  1, "Drug A", 5, 5,
  2, "Drug B", 2, 0,
  2, "Drug B", 2, 2,
  3, "Drug A", 4, 0,
  3, "Drug A", 4, 4,
  4, "Drug B", 7, 0,
  4, "Drug B", 7, 7
)

mk1_data <- tibble::tribble(
  ~"id", ~"type", ~"time",
  1, "Dose II", 0,
  1, "Dose II", 1.5,
  2, "Dose II", 2,
  3, "Dose I", 0,
  3, "Dose II", 0.5,
  3, "Dose II", 1,
  3, "Dose I", 1.25,
  4, "Dose II", 2,
  4, "Dose I", 3,
  4, "Dose I", 7
)

mk2_data <- tibble::tribble(
  ~"id", ~"label", ~"name", ~"time",
  1, "💊", "A", 1,
  1, "💉", "B", 2,
  2, "💉", "B", 3,
  4, "💉", "B", 6
)

ggswim_layer <- ggswim(data = pt_data, aes(x = time, y = id, fill = "trt"))

test_that("add_marker works for aes mapping", {
  mk1_layer <- add_marker(
    data = mk1_data,
    mapping = aes(x = time, y = id, color = type)
  )

  expect_setequal(class(mk1_layer), c("LayerInstance", "Layer", "ggproto", "gg"))
  expect_true("swim_class" %in% names(attributes(mk1_layer)))
  expect_true(attributes(mk1_layer)$swim_class == "marker_point")

  # Simple layer with aesthetic mapping ----
  p1 <- ggswim(data = pt_data, aes(x = time, y = id, fill = "trt")) +
    mk1_layer

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Simple marker addition on pt data",
    fig = p1
  )

  # Emoji layer aesthetic mapping ----
  p2 <- ggswim_layer +
    mk1_layer +
    add_marker(data = mk2_data, aes(x = time, y = id, label = label, colour = name))

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Addition of labels layer, test for expected legend",
    fig = p2
  )
})

test_that("add_marker works for static aesthetics", {
  mk1_layer <- add_marker(
    data = mk1_data[mk1_data$type == "Dose I", ],
    mapping = aes(x = time, y = id, name = "Dose Name"),
    color = "red", size = 10
  ) |>
    suppressWarnings()

  p1 <- ggswim_layer +
    mk1_layer

  skip_on_ci()
  vdiffr::expect_doppelganger(
    title = "Single marker with static color callout",
    fig = p1
  )
})

test_that("error on fill argument", {
  expect_error(
    add_marker(data = pt_data, aes(x = time, y = id, fill = trt)),
    class = "unsupported_aes"
  )
})

test_that("warn on label without color argument", {
  expect_warning(
    add_marker(data = mk2_data, aes(x = time, y = id, label = trt)),
    class = "marker_label_aes"
  )
})
