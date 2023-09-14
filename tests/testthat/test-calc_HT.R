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

  # fill NA weather temperatures
  w <- data.table::copy(nt_weather)
  library(epiphytoolR)

  # set the width of the rolling window, this will impact the smoothing of the
  #  imputation
  rolling_window <- 60
  # set the index in the table, this is needed for the rolling apply function
  w[,indx := .I]

  # check how many NAs are in the temperature column
  w[is.na(temp),indx]

    # Rolling apply using the impute_fill function on temperature variable
  w[, tm_imp := round(data.table::frollapply(
     indx,
     n = rolling_window,
     fill = NA_real_,
     FUN = impute_fill,
     FUN_n = rolling_window,
     times = times,
     var = temp,
     align = "center"
     ),3)]


  # visualise the fit of the fill
  plot(w$temp[1050:1600], type = "l")
  lines(w$tm_imp[1050:1600], type = "l", col = "blue")
  abline(v = seq(0,550, by = 24))

  # set the NAs in temperature with the estimated temperature
  w[is.na(temp), temp:= tm_imp]

  # Check how many NAs remain
  w[is.na(temp),indx]

  # We could widen the rolling_window, so we only need to impute once or we can
  #  run the function twice.
  # we can run the impute_fill function over the same data to fill in the rest
  dif <- 20
  while(dif > 0) {
    #get nas
    na_s <- length(w[is.na(temp), indx])
    w[, tm_imp := round(
      data.table::frollapply(
        indx,
        n = rolling_window,
        fill = NA_real_,
        FUN = impute_fill,
        FUN_n = rolling_window,
        times = times,
        var = temp,
        align = "center"
      ),
      3
    )]

    # set the NAs in temperature with the estimated temperature
    w[is.na(temp), temp := tm_imp]


    # Check how many NAs remain
    dif <- na_s - length(w[is.na(temp), indx])
    cat("Dif remaining: ", dif,"\n")
  }
  # visualise the fit of the fill
  plot(w$temp[2200:2243], type = "l")
  lines(w$tm_imp[2200:2243], type = "l", col = "blue")
  abline(v = seq(0,43, by = 24))


  # Impute Relative humidity _____________________
  dif <- length(w[is.na(rh), indx])
  while(dif > 0) {
    #get nas
    na_s <- length(w[is.na(rh), indx])
    w[, rh_imp := round(
      data.table::frollapply(
        indx,
        n = rolling_window,
        fill = NA_real_,
        FUN = impute_fill,
        FUN_n = rolling_window,
        times = times,
        var = rh,
        align = "center"
      ),
      3
    )]
    # set the NAs in temperature with the estimated temperature
    w[is.na(rh), rh := rh_imp]
    # Check how many NAs remain
    dif <- na_s - length(w[is.na(rh), indx])
    # Report
    cat("Dif remaining: ", dif,"\n")
  }



  # check how many NA entries remain and trim them
  w[, .(temp_nas = sum(is.na(temp)),
        rh_nas = sum(is.na(rh)))]
  dim(w)
  # Remove lines with NAs
  w <- w[1:(which(is.na(temp))[1]-1)]
  dim(w)


  # run model
  w <- estimate_DM_PI(w = w)
  # calculate hydrothemal time
  w[, HT_h := cumsum(calc_HT(temp,M_h))]

})
