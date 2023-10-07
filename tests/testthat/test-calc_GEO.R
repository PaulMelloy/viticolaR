test_that("cumulative survivability GEO is calculated", {
  sus_v <- calc_SUS(rep(20:35, each = 10),95)

  expect_no_error(calc_GEO(
    SUS_h = sus_v,
    cohort = rep(c(rep(0, 5), 1, 1, 1,
                   rep(0, 2), 0,
                   rep(0, 4), 0), 10),
    PMO = rep(1, 160)
  ))
})
