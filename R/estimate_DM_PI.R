#' Estimate Downy Mildew Primary Infections
#'
#' Umbrella function to calculate the number of primary infections on grapevine
#' leaves from overwintering oospores as described by
#' \insertRef{rossi_mechanistic_2008}{viticolaR}
#'
#' @param w hourly weather data.table, class epiphy.weather.
#' @param SOD Proportion of seasonal oospores dose. This starts as 1 in the middle
#'  of winter, the first of June for the southern hemisphere or the 1st of January
#'  in the northern hemisphere. Also refered to as 'morphologically mature oospores
#'  (MMO).
#'
#' @return
#' @export
#' @import data.table
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#'
#' @examples
estimate_DM_PI <- function(w,
                           Start = "detect",
                           End = "detect",
                           SOD = 1){
  # data.table global binding variable
  times <- vpd <- rh <- temp <- M_h <- rain <- HT_h <- DOR <- PMO <- NULL

  if(inherits(w,"epiphy.weather") == FALSE) stop("'w' must be class epiphy.weather
                                              use 'format_weather()'")

  if(length(unique(w$station)) > 1)stop("Model does not currently support weather
                                             data with multiple sources of weather
                                             station data. Please select a single
                                             weather station source")
  # The second state variable is represented by the morphologically mature
  #  oospores (MMO): the change from SOD to MMO depends on the day of the year
  #  (DOY). Since oospores reach this stage in November, irrespective of the period
  #  of their formation (Vercesi et al., 1999b), the model assumes that the entire
  #  oospore population is in the MMO stage on the 1st of January (DOY = 1) at
  #  01.00 h (h = 1, with h being the counter for the hours). Therefore, when h =1,
  #  MMO = SOD.
  MMO <- SOD

  # set start time
  # this is 1st of January in northern hemisphere, and 1st of July in the south
  if(Start == "detect"){
    Yr <- data.table::year(w$times[1])
    if(w$times[1] < as.POSIXct(paste0(Yr,"-07-01 00:00:00"),tz = "UTC")){
      Start <- as.POSIXct(paste0(Yr,"-07-01 00:00:00"),tz = "UTC")
    }else{
      Start <- w$times[1]
    }
  }

  if(End == "detect"){
    End <-
      data.table::fifelse(w[.N,times] < as.POSIXct(paste0(Yr + 1,"-05-29 23:00:00"),tz = "UTC"),
              w[.N,times],
              as.POSIXct(paste0(Yr + 1,"-05-29 23:00:00"),tz = "UTC"))
  }
  # select weather data
  w <- w[times >= Start &
           times <= End]

  if(any(is.na(w$temp)))stop("NA temperature values detected in weather, please correct,
                              use epiphytoolR::impute_fill() or epiphytoolR::impute_diurnal()")

  # add VPD column
  w[, vpd := epiphytoolR::calc_vpd(rh,temp)]

  if(any(is.na(w$rain)))stop("rainfall contains 'NA' values. Please amend")

  # Leaf litter moisture sufficient for oospore maturation
  # dichotomic variable
  w[, M_h := data.table::fcase(rain > 0, 1,
                               as.numeric(vpd) <= 0.45,1,
                               as.numeric(vpd) > 0.45 & rain == 0,0,
                               default = 0)]

  # Calculate cumulative hydrothermal time
  w[, HT_h := cumsum(calc_HT(temp,M_h))]

  # calculate rate at which dormancy breaks
  # equation 2
  w[, DOR := exp(-15.891 * exp(-0.653*(HT_h + 1)))]

  #calculate physiological maturity
  w[, PMO := MMO * DOR]

  # Index oospore cohort
  # calc_PMO_cohort()
  w[, J_cohort := 0] # initiate the variable
  for(i in seq_along(w$J_cohort)){
    if(i == 1) next
    c1 <- fifelse(w[i-1,rain] < 0.2 & w[i,rain] >= 0.2,
                  w[i,J_cohort] +1,
                  w[i,J_cohort])
    w[i:.N, J_cohort := c1]
  }

  # Specify dry hours
  # w[, J_cohort := fifelse(rain == 0, 0, J_cohort)]

  oospore_cohorts <- unique(w$J_cohort[w$J_cohort != 0])

  cohort_list <-
    lapply(oospore_cohorts, function(oo_cohort){

      oo_cohort <- as.numeric(oo_cohort)
      PMO_c <- w[which(J_cohort == oo_cohort - 1),last(PMO)]

      # get all weather data from an hour before cohort starts to the end of the data
      w_c <- data.table(indx = w[which(oo_cohort == J_cohort)[1]:.N, indx],
                        times = w[which(oo_cohort == J_cohort)[1]:.N, times],
                        temp = w[which(oo_cohort == J_cohort)[1]:.N, temp],
                        rh = w[which(oo_cohort == J_cohort)[1]:.N, rh],
                        rain = w[which(oo_cohort == J_cohort)[1]:.N, rain],
                        vpd = w[which(oo_cohort == J_cohort)[1]:.N, vpd],
                        M_h = w[which(oo_cohort == J_cohort)[1]:.N, M_h],
                        PMO = w[which(oo_cohort == J_cohort)[1]:.N, PMO],
                        J_c = w[(which(oo_cohort == J_cohort)[1]):.N, J_cohort])

      epsilon <- w[which(oo_cohort == J_cohort), first(indx)]

      # calculate germinating Oospores per hour by cohort
      w_c[,GER := cumsum(calc_GER(M_h,temp))]

      # get the number of oospores when
      GEO_c <- PMO_c
      GER_c_h <- w_c[GER >= 1, first(indx)]

      # calculate surviving sporangia in cohort
      w_c[indx >= GER_c_h,
          SUS_h := cumsum(calc_SUS(temp,rh))]

    # get cohort sporangia survival time
      SUS_c_h <- w_c[indx >= GER_c_h &
                       SUS_h <= 1, last(indx)]

      # Is there a zoospore release for this cohort?
      w_c[,ZooWindow := fifelse(indx >= GER_c_h & indx <= SUS_c_h,
                                TRUE,FALSE)]
      # Is there a zoospore release for this cohort?
      ## Initialise
      w_c[, REL := FALSE]
      w_c[ZooWindow==TRUE,
          REL := zsp_release(WD_h = cumsum(M_h),
                             TWD_h = cumsum(temp)/seq_along(temp))]

      # Germinated oospores making up the surviving germinated oopspores from cohort
      # w_c[, GEO_h := fifelse(GER >=1, GEO_c, PMO_c)]
      # w_c[, GEO_h := fifelse(SUS_h > 1, 0, GEO_h)]
      # GEO <- w_c[J_c == oo_cohort, last(GEO_h)]

      # get p (hour of zoospore release)
      zoo_release_ind <- w_c[REL == TRUE,first(indx)]

      # init SUZ; Zoospore survival
      w_c[,SUZ_h := NA_real_]

      # if zoospores don't survive return NA
      if(length(zoo_release_ind) == 0){
        w_c[, c("ZRE_h",
                "INC_h",
                "ZDI_h") := list(FALSE,FALSE,FALSE)]
       return(list(cohort = oo_cohort,
                   w_c = w_c,
                   spo_germination_hour = GER_c_h,
                   spo_death_hour = SUS_c_h,
                   zoo_release_ind = NA_integer_,
                   zoo_dispersal_ind = NA_integer_,
                   zoo_infection_ind = NA_integer_,
                   INC_h_lower = NA_integer_,
                   INC_h_upper = NA_integer_,
                   PMO_c = NA))
      }

      # calculate the zoospore survival
      SUZ <- w_c[indx >= zoo_release_ind,
                 cumsum(indx - zoo_release_ind) /
                           cumsum(shift(M_h, n = 1, type = "lead"))]
      w_c[indx >= zoo_release_ind,
          SUZ_h := SUZ]

      # Determine zoospore release
      ## ZRE_h is the number of zoospores relseased at hour _h
      w_c[,ZRE_h := fifelse(SUZ_h <= 1 &
                              SUZ_h >= 0,
                           TRUE, FALSE)]

      # Determine if a zoospore dispersal has occured
      w_c[, ZDI_h := fifelse(rain >= 0.2 & ZRE_h == TRUE,
                           TRUE,
                           FALSE)]

      # is equivalent to lowercase greek delta (ZDI_delta)
      zoo_dispersal_ind <- w_c[ZDI_h == TRUE,first(indx)]

      ## EXIT if ...
      # if zoospores don't survive return NA
      if(length(zoo_dispersal_ind) == 0){
        w_c[, c("ZDI_h",
                "INC_h") := list(FALSE,FALSE)]
        return(list(cohort = oo_cohort,
                    w_c = w_c,
                    spo_germination_hour = GER_c_h,
                    spo_death_hour = SUS_c_h,
                    zoo_release_ind = zoo_release_ind,
                    zoo_dispersal_ind = NA_integer_,
                    zoo_infection_ind = NA_integer_,
                    INC_h_lower = NA_integer_,
                    INC_h_upper = NA_integer_,
                    PMO_c = NA))
      }

      # Determine if zoospores successfully infect INF_h
      # initialise INF_h
      w_c[,INF_h := FALSE]
      w_c[indx >= zoo_dispersal_ind,
          INF_h := fifelse(cumsum(M_h)*(cumsum(temp)/seq_along(temp)) >= 60,
                          TRUE,FALSE)]

      # is equivalent to lowercase greek iota (ZIN_iota)
      # hour of zoospore infection
      zoo_infection_ind <- w_c[INF_h == TRUE,first(indx)]

      ## EXIT if ...
      #  zoospores don't infect return NA
      if(length(zoo_infection_ind) == 0){
        w_c[, c("INC_h") := list(FALSE)]
        return(list(cohort = oo_cohort,
                    w_c = w_c,
                    spo_germination_hour = GER_c_h,
                    spo_death_hour = SUS_c_h,
                    zoo_release_ind = zoo_release_ind,
                    zoo_dispersal_ind = zoo_dispersal_ind,
                    zoo_infection_ind = NA_integer_,
                    INC_h_lower = NA_integer_,
                    INC_h_upper = NA_integer_,
                    PMO_c = NA))
      }

      # calculate min and max periods of incubation
      w_c[indx >= zoo_infection_ind,
          INC_l := cumsum(1/(24*(45.1 - 3.45 * temp + 0.073 * (temp^2))))]
      w_c[indx >= zoo_infection_ind,
          INC_u := cumsum(1/(24*(59.9 - 4.55 * temp + 0.095 * (temp^2))))]

      INC_h_lower <- w_c[INC_l <= 1, last(indx)]
      INC_h_upper <- w_c[INC_u <= 1, last(indx)]


      return(list(cohort = oo_cohort,
                  w_c = w_c,
                  spo_germination_hour = GER_c_h,
                  spo_death_hour = SUS_c_h,
                  zoo_release_ind = zoo_release_ind,
                  zoo_dispersal_ind = zoo_dispersal_ind,
                  zoo_infection_ind = zoo_infection_ind,
                  INC_h_lower = INC_h_lower,
                  INC_h_upper = INC_h_upper,
                  PMO_c = PMO_c))

    })


  m_viticola_out <- list(cohort_list = cohort_list,
                         w = w,
                         time_hours = w$times,
                         Hyd_t = w$HT_h,
                         PMO = w$PMO,
                         cohorts = max(oospore_cohorts)
  )
  class(m_viticola_out) <- c("m_viticola",class(m_viticola_out))

  return(m_viticola_out)


  # The HT is also used to calculate the length of the primary inoculum season
  #  (PIS): PIS starts and finishes when HT = 1.3 and 8.6, respectively (Fig. 3).
  # These values correspond to the time when 3% and 97% of the total oospores
  #  forming the SOD have entered the PMO stage, respectively, according to Eq. (3)
  # plot(w$times, w$PMO, type="l")
  # plot(w$times, w$HT_h, type="l")
}# end of estimate_DM_PLR




