test_that("Conveniance function returns expected", {
  Tmod <- estimate_DM_PI(w = nt_weather)

  T1 <- get_PI_dates(mod = Tmod)

  expect_s3_class(T1,"data.table")
  expect_equal(dim(T1),c(140,3))
  expect_type(T1$cohort,"integer")
  expect_s3_class(T1$primary_infection_stage,"factor") # factor
  expect_s3_class(T1$hour,"POSIXct") # POSIXct

  # library(ggplot2)
  # T1[primary_infection_stage != "spo_death_hour" &
  #      is.na(hour) == FALSE] |>
  # ggplot(data = , aes(x = hour, y = primary_infection_stage, group = factor(cohort)))+
  #   geom_line()

})
