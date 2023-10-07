test_that("Testing phase of model", {
  # library(devtools)
  # library(epiphytoolR)
  T1 <- estimate_DM_PI(w = nt_weather)

  expect_type(T1, "list")
  expect_equal(length(T1),7)
  expect_equal(length(T1$cohort_list),20)
  expect_equal(unlist(lapply(T1$cohort_list,"[[",1)), 1:20)
  expect_type(do.call("c",lapply(T1$cohort_list,"[[","spo_germination_hour")),
                  "integer") # POSIXct ???
  expect_type(do.call("c",lapply(T1$cohort_list,"[[","spo_death_hour")),
              "integer") # POSIXct ???
  expect_equal(do.call("c",lapply(T1$cohort_list,"[[","spo_germination_hour"))[1:2],
              c(1401,1403))
  expect_equal(do.call("c",lapply(T1$cohort_list,"[[","spo_death_hour"))[1:2],
               c(1516,1516))
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","zoo_release_ind"))) == FALSE),
               17)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","zoo_dispersal_ind")))),
               19)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","zoo_infection_ind")))),
               19)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_lower")))),
               19)
  expect_equal(sum(is.na(do.call("c",lapply(T1$cohort_list,"[[","INC_h_upper")))),
               19)

  expect_equal(length(T1$time_hours),1455)
  expect_is(T1$time_hours[1],"POSIXct")

  expect_s3_class(T1$cohort_list[[1]]$w_c, "data.table")

  inf_progress <- function(x){
    c(T1$cohort_list[[x]]$spo_germination_hour,
      T1$cohort_list[[x]]$spo_death_hour,
      T1$cohort_list[[x]]$zoo_release_ind,
      T1$cohort_list[[x]]$zoo_dispersal_ind,
      T1$cohort_list[[x]]$zoo_infection_ind
    )
  }
  expect_false(inf_progress(1)[1] == inf_progress(1)[2])
  expect_true(inf_progress(1)[1] < inf_progress(1)[2])

  for(i in 1:20){
    print(inf_progress(i))
    Sys.sleep(1)
  }

T1$cohort_list[[9]]$spo_germination_hour
#T1$cohort_list[[9]][c("w_c")]
T1$cohort_list[[9]]$INC_h_lower
T1$cohort_list[[9]]$INC_h_upper


})

