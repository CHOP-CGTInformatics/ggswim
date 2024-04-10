test_that("grid.draw method works", {
  out <- grid.draw.ggswim_obj(ggswim(
    patient_data,
    aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    )
  ))
  expect_setequal(class(out), c("gg", "ggplot"))
})
