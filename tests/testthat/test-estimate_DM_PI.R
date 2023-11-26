test_that("Testing phase of model", {
  # library(devtools)
  # library(epiphytoolR)
  T1 <- estimate_DM_PI(w = nt_weather)

  expect_type(T1, "list")
  expect_equal(length(T1),7)
  expect_equal(length(T1$cohort_list),20)
  expect_equal(unlist(lapply(T1$cohort_list,"[[",1)), 1:20)
  expect_type(do.call("c",lapply(T1$cohort_list,"[[","GEO_h")),
                  "double") # POSIXct ???
  expect_type(do.call("c",lapply(T1$cohort_list,"[[","SUS_death_h")),
              "double") # POSIXct ???
  expect_equal(do.call("c",lapply(T1$cohort_list,"[[","GEO_h"))[1:2],
              c(636,638))
  expect_equal(do.call("c",lapply(T1$cohort_list,"[[","SUS_death_h"))[1:2],
               c(711,713))
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","ZRE_ind"))) == FALSE),
               17)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","ZDI_ind")))),
               19)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","ZIN_ind")))),
               20)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_lower")))),
               20)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_upper")))),
               20)
  # Might need a test here that leads to an infection

  expect_equal(length(T1$time_hours),1455)
  expect_is(T1$time_hours[1],"POSIXct")

  expect_s3_class(T1$cohort_list[[1]]$w_c, "data.table")

  inf_progress <- function(x){
    c(T1$cohort_list[[x]]$GEO_h,
      T1$cohort_list[[x]]$SUS_death_h,
      T1$cohort_list[[x]]$ZRE_ind,
      T1$cohort_list[[x]]$ZDI_ind,
      T1$cohort_list[[x]]$ZIN_ind
    )
  }
  expect_false(inf_progress(1)[1] == inf_progress(1)[2])
  expect_true(inf_progress(1)[1] < inf_progress(1)[2])

  # slowly print each cohort outcomes
  # for(i in 1:20){
  #   print(inf_progress(i))
  #   Sys.sleep(1)
  # }


  expect_equal(T1$cohort_list[[9]]$GEO_h, 861)
  expect_equal(T1$cohort_list[[9]]$INC_h_lower, NA_integer_)
  expect_equal(T1$cohort_list[[9]]$INC_h_upper, NA_integer_)


})

T2 <- estimate_DM_PI(nt_weather)
test_that("Indx and hours match",{
  expect_equal(T2$start_time,as.POSIXct("2023-07-01","UTC"))
  expect_equal(T2$w[,first(times)],as.POSIXct("2023-07-01","UTC"))
  expect_equal(T2$w[,first(indx)],0)
  expect_equal(T2$w[,last(indx)],as.integer(difftime(last(T2$time_hours),
                                                     first(T2$time_hours),units = "hours")))
  expect_equal(T2$cohort_list[[10]]$w_c[,last(indx)],
               as.integer(difftime(last(T2$cohort_list[[10]]$w_c$times),
                                   as.POSIXct("2023-07-01","UTC"),units = "hours")))
})

