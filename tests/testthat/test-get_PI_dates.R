test_that("Conveniance function returns expected", {
  Tmod <- estimate_DM_PI(w = nt_weather,
                         Start = "detect",
                         End = "detect")

  T1 <- get_PI_dates(mod = Tmod)

  expect_s3_class(T1,"data.table")
  expect_equal(dim(T1),c(160,3))
  expect_type(T1$cohort,"integer")
  expect_s3_class(T1$primary_infection_stage,"factor") # factor
  expect_s3_class(T1$hour,"POSIXct") # POSIXct



  T2 <- get_PI_dates(mod = Tmod)


  # library(ggplot2)
  # T1[primary_infection_stage != "SUS_death_h" &
  #      is.na(hour) == FALSE] |>
  # ggplot(data = , aes(x = hour, y = primary_infection_stage, group = factor(cohort)))+
  #   geom_line()

})
