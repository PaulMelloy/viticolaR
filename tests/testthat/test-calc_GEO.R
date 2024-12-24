test_that("cumulative survivability GEO is calculated", {
  sus_v <- calc_SUS(rep(20:35, each = 10),95)

  expect_no_error(
    calc_GEO(SUS_h = sus_v,
             cohort = rep(c(rep(0, 5), 1, 1, 1, rep(0, 2), 0, rep(0, 4), 0), 10),
             PMO = rep(1, 160)))

  germ_oospores <- calc_GEO(SUS_h = sus_v,
                            cohort = rep(c(rep(0, 5), 1, 1, 1, rep(0, 2), 0, rep(0, 4), 0), 10),
                            PMO = rep(1, 160))

  expect_equal(germ_oospores,sus_v)
  expect_type(germ_oospores, "double")
  expect_length(germ_oospores,length(sus_v))
  expect_true(all(germ_oospores < 1))

})

test_that("Test calc_GEO function: unexpected error",{
  # Initialize test data
  SUS_h <- c(1, 2)
  PMO <- c(1, 1)
  cohort <- rep(1, each = 3)

  # Test with different argument lengths
  expect_error(calc_GEO(PMO, SUS_h, cohort), "all argument lengths must match")

  # NA handling
  # PMO <- rep(c(NA, 1))
  # cohort <- rep(1, each = 2)
  # expect_error(calc_GEO(PMO, SUS_h, cohort), "all argument lengths must match")
})
