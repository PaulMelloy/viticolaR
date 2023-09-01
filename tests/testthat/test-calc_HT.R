test_that("calc_HT returns expected results", {
  expect_equal(calc_HT(1,1),0.0008220064, tolerance = 0.000000001)
  expect_equal(calc_HT(-1,1),0, tolerance = 0.000000001)
  expect_equal(calc_HT(24,1),0.01855783, tolerance = 0.00000001)

  expect_error(calc_HT(24:30,1),regexp = "Tm_h and M_h must have the same length")
  expect_equal(calc_HT(24:30,c(1,1,1,1,1,0,0)),
               c(0.018557834, 0.015071590, 0.011895472, 0.009342965,
                 0.007393693, 0.000000000, 0.000000000), tolerance = 0.00000001)
  expect_equal(calc_HT(30:35,rep(0,6)),
               rep(0,6))


})
