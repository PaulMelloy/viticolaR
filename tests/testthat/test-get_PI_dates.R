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



  T2 <- estimate_DM_PI(nt_weather,
                       Start = "2023-07-01",
                       End = "2023-08-30")

  # get PI_dates summary
  PI_dates <- get_PI_dates(T2)

  expect_equal(as.character(stats::na.exclude(
    PI_dates[primary_infection_stage == "GEO_h", unique(as.Date(hour))])),
    c("2023-07-27", "2023-07-29", "2023-07-31", "2023-08-05", "2023-08-06",
      "2023-08-21", "2023-08-23", "2023-08-24", "2023-08-26", "2023-08-27"))

  expect_equal(as.character(stats::na.exclude(PI_dates[primary_infection_stage == "ZDI_ind", unique(as.Date(hour))])),
               "2023-08-06")
  expect_equal(as.character(stats::na.exclude(PI_dates[primary_infection_stage == "INC_h_lower", unique(as.Date(hour))])),
               "2023-08-15")
  expect_equal(as.character(stats::na.exclude(PI_dates[primary_infection_stage == "INC_h_upper", unique(as.Date(hour))])),
               "2023-08-18")
  expect_equal(as.character(stats::na.exclude(PI_dates[primary_infection_stage == "ZIN_ind", unique(as.Date(hour))])),
               "2023-08-06")


  # library(ggplot2)
  # T1[primary_infection_stage != "SUS_death_h" &
  #      is.na(hour) == FALSE] |>
  # ggplot(data = , aes(x = hour, y = primary_infection_stage, group = factor(cohort)))+
  #   geom_line()

})
