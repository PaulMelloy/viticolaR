# Get model
ntw <- estimate_DM_PI(
  w = nt_weather,
  Start = as.POSIXct("2023-07-01"),
  End = as.POSIXct("2023-08-30")
)

test_that("geom_ribbon works",{
  p1g <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,,y = "GER")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p1, "gg")

  p1p <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "PMO")
  p12g <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "GEO")


})

test_that("plot weather works",{
  p3 <-
    plot_weather(ntw,4)


  expect_s3_class(p3, "ggplot")
  expect_s3_class(p2, "gg")
})


