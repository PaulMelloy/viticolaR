test_that("Testing phase of model", {
  T1 <- estimate_DM_PI(w = nt_weather,
                       Start = "2023-07-01",
                       End = "2023-08-30")

  expect_type(T1, "list")
  expect_equal(length(T1),7)
  expect_equal(length(T1$cohort_list),18)
  expect_equal(unlist(lapply(T1$cohort_list,"[[",1)), 1:18)
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
               15)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","ZIN_ind")))),
               15)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_lower")))),
               15)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_upper")))),
               15)
  # Might need a test here that leads to an infection

  expect_equal(length(T1$time_hours),1441)
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

#  slowly print each cohort outcomes
  # for(i in 1:18){
  #   print(inf_progress(i))
  #   Sys.sleep(1)
  # }


  expect_equal(T1$cohort_list[[9]]$GEO_h, 861)
  expect_equal(T1$cohort_list[[9]]$INC_h_lower, 1082)
  expect_equal(T1$cohort_list[[9]]$INC_h_upper, 1153)


})

T2 <- estimate_DM_PI(nt_weather,
                     Start = "2023-07-01",
                     End = "2023-08-30")
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

test_that("summary.m_viticolR returns expected output",{

  expect_no_error(summary(T2))

  # Model run time
  expect_equal(T2$time_hours[1],
               as.POSIXct("2023-07-01","UTC"))
  expect_equal(T2$time_hours[length(T2$time_hours)],
               as.POSIXct("2023-08-30","UTC"))
  expect_equal(round(length(T2$w$times)/24,1),
               60)
  expect_equal(unique(T2$w$station),
               "PORTABLE QFRJ (North Tamborine)")
  expect_equal(T2$cohorts,
               18)
  expect_equal(T2$PMO[length(T2$PMO)],
               0.2967689,tolerance = 0.0000001)


})


test_that("list element classes are expected",{
  # check character dates are accepted
  Tmod <- estimate_DM_PI(w = nt_weather,
                         Start = "2023-07-01",
                         End = "2023-08-29")

  expect_s3_class(Tmod$start_time, "POSIXct")
  expect_true(format(Tmod$start_time, format = "%Z") == "UTC")
  expect_s3_class(Tmod$time_hours, "POSIXct")
  expect_true(unique(format(Tmod$time_hours, format = "%Z")) == "UTC")

  })

test_that("we can plot a the output",{

  expect_no_condition(
    ggplot2::ggplot()+
      geom_ribbon_viticolR(T2)
  )


})

