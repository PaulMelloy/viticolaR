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

  expect_s3_class(p1g, "ggplot")
  expect_s3_class(p1g, "gg")

  p1p <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "PMO")
  expect_s3_class(p1p, "ggplot")
  expect_s3_class(p1p, "gg")

  p12g <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "GEO")
  expect_s3_class(p12g, "ggplot")
  expect_s3_class(p12g, "gg")

  p1s <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "SUS_h")
  expect_s3_class(p1s, "ggplot")
  expect_s3_class(p1s, "gg")

  p1z <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "SUZ_h",x_subset = "ZRE_h")
  expect_s3_class(p1z, "ggplot")
  expect_s3_class(p1z, "gg")

  p1z <-
    ggplot2::ggplot() +
    geom_ribbon_viticolaR(ntw,y = "SUZ_h")
  expect_s3_class(p1z, "ggplot")
  expect_s3_class(p1z, "gg")

})

test_that("plot weather works",{
  p3 <-
    plot_weather(ntw,4)


  expect_s3_class(p3, "ggplot")
  expect_s3_class(p3, "gg")
})

test_that("plot weather filters by date",{
  expect_no_condition(
    plot_weather(ntw,4,
                 date_min = as.POSIXct("2023-07-01"),
                 date_max = as.POSIXct("2023-08-30"))
    )
  p3 <-
    plot_weather(ntw,4,
                 date_min = as.POSIXct("2023-07-01"),
                 date_max = as.POSIXct("2023-08-30"))


  expect_s3_class(p3, "ggplot")
  expect_s3_class(p3, "gg")
})

