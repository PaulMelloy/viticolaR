test_that("Testing phase of model", {
  # library(devtools)
  library(epiphytoolR)
  T1 <- estimate_DM_PI(w = nt_weather)

  expect_equal(length(T1),29)
  expect_equal(sum(is.na(T1) == FALSE),3)
  expect_type(T1, "list")
  expect_s3_class(T1[[5]], "data.table")

  T1[[5]]

})
