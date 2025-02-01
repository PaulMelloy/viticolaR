#' Estimate Downy Mildew Primary Infections
#'
#' Umbrella function to calculate the number of primary infections on grapevine
#' leaves from overwintering oospores as described by
#' \insertRef{rossi_mechanistic_2008}{viticolR}
#'
#' @param w hourly weather data.table, class epiphy.weather.
#' @param SOD Proportion of seasonal oospores dose. This starts as 1 in the middle
#'  of winter, the first of June for the southern hemisphere or the 1st of January
#'  in the northern hemisphere. Also referred to as 'morphologically mature oospores
#'  (MMO).
#' @param Start POSIXct, date time in UTC when to start the model running. By default
#'  the function will 'detect' the start of the model. This model is configured
#'  for the southern hemisphere, so the model will start on the first of July of
#'  the first year of weather data input into the model 'w'.
#' @param End POSIXct, date time in UTC when to end the model running. By default
#'  the function will 'detect' the end of the model. This model is configured
#'  for the southern hemisphere, so the model will End on the last day of June in
#'  the first season year of the input weather data 'w'.
#'
#' @return list, class == m_viticolR, containing the model output and downy mildew
#'  primary infection progress.
#'  \tabular{rl}{
#'   **start_time**: \tab Time in POSIXct format with "UTC" time-zone\cr
#'   **time_hours**: \tab vector of timesteps the model ran \cr
#'   **Hyd_t**: \tab vector of hydrothermal time corresponding to each time_hours \cr
#'   **PMO**: \tab vector of physiological mature oospores at each time step \cr
#'   **cohorts**: \tab total number of oospore cohorts which germinated as a result
#'    of weather conditions meeting the threshold\cr
#'   **w**: \tab weather data.table of class `epiphy.weather` used in the model \cr
#'   **cohort_list**: \tab List of model outputs for each cohort \cr
#'   **lat**: \tab Station latitude in decimal degrees \cr
#'   **station**: \tab Unique station identifying name \cr
#'   **YYYY**: \tab Year \cr
#'   **MM**: \tab Month \cr
#'   **DD**: \tab Day \cr
#'   **hh**: \tab Hour \cr
#'   **mm**: \tab Minute \cr}
#' @export
#' @import data.table
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#'
#' @examples
#' mod1 <- estimate_DM_PI(w = nt_weather,
#'                        Start = as.Date("2023-07-01"),
#'                        End = as.Date("2023-08-30"))
estimate_DM_PI <- function(w,
                           Start = "detect",
                           End = "detect",
                           SOD = 1){
  # data.table global binding variable
  times <- vpd <- rh <- temp <- M_h <- rain <- HT_h <- DOR <- PMO <- J_cohort <-
    indx <- GER <- SUS_h <- ZooWindow <- REL <- SUZ_h <- ZRE_h <- ZDI_h <-
    INF_h <- INC_l <- GEO <- INC_u <- NULL

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

  if("Date" %in% class(Start)) Start <- as.POSIXct(Start)
  if("Date" %in% class(End)) End <- as.POSIXct(End)

  # set start time
  # this is 1st of January in northern hemisphere, and 1st of July in the south
  if(as.character(Start) == "detect"){
    season_yr <- detect_season(w)

    current_yr <- fifelse(data.table::month(Sys.time()) < 7,
                          data.table::year(Sys.time()) -1,
                          data.table::year(Sys.time()))

    if(w$times[1] < as.POSIXct(paste0(season_yr,"-07-01 00:00:00"),tz = "UTC")){
      Start <- as.POSIXct(paste0(season_yr,"-07-01 00:00:00"),tz = "UTC")
    }else{
      Start <- w$times[1]
    }
  }else{
    if(is.character(Start)){
      Start <- as.POSIXct(Start, tz = "UTC")
      }
  }

  if(as.character(End) == "detect"){
    End <-
      data.table::fifelse(
        w[.N,times] < as.POSIXct(paste0(season_yr + 1,"-05-29 23:00:00"),tz = "UTC"),
              w[.N,times],
              as.POSIXct(paste0(season_yr + 1,"-05-29 23:00:00"),tz = "UTC"))
  }else{
    if(is.character(End)){
      End <- as.POSIXct(End, tz = "UTC")
      }
  }

  # select weather data
  w <- w[times >= Start &
           times <= End]

  if(nrow(w)== 0) stop("supplied weather data is outside 'Start' and 'End' dates.
                       'detect' is using ",Start, " & ",End)

  # reinitialise indx
  w[,indx := (1:.N)-1]

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

  # calculate the response of each PMO cohort
  cohort_list <-
    lapply(oospore_cohorts, function(oo_cohort){

      oo_cohort <- as.numeric(oo_cohort)
      # Get number of physiological mature oospores ready to germinate in this
      #  cohort
      PMO_c <- w[which(J_cohort == oo_cohort - 1),last(PMO)]

      # get all weather data from an hour before cohort starts to the end of the data
      #  This includes weather data for future cohorts
      w_c <- data.table(indx = w[which(oo_cohort == J_cohort)[1]:.N, indx],
                        times = w[which(oo_cohort == J_cohort)[1]:.N, times],
                        temp = w[which(oo_cohort == J_cohort)[1]:.N, temp],
                        rh = w[which(oo_cohort == J_cohort)[1]:.N, rh],
                        rain = w[which(oo_cohort == J_cohort)[1]:.N, rain],
                        vpd = w[which(oo_cohort == J_cohort)[1]:.N, vpd],
                        M_h = w[which(oo_cohort == J_cohort)[1]:.N, M_h],
                        PMO = w[which(oo_cohort == J_cohort)[1]:.N, PMO],
                        J_c = w[which(oo_cohort == J_cohort)[1]:.N, J_cohort])

      # get first hour in cohort
      #  This is also when sporangia start germinating from oospores in leaf litter
      epsilon <- w[which(oo_cohort == J_cohort), first(indx)]

      # calculate germinating Oospores per hour by cohort
      w_c[,GER := cumsum(calc_GER(M_h,temp))]

      # get the number of physiologically mature oospores which have germinated
      GEO_c <- PMO_c
      # Get the time when all sporangia have finished germinating from oospores
      GER_c_h <- w_c[GER >= 1, first(indx)]

      # if germination has not occured yet for cohort return NAs
      if(any(w_c$GER >= 1)== FALSE){
        w_c[, c("SUS_h",
                "GEO",
                "ZooWindow",
                "REL",
                "SUZ_h",
                "ZRE_h",
                "INC_h",
                "ZDI_h",
                "INF_h") := list(NA_real_,0,FALSE,FALSE,NA_real_,
                                 FALSE,FALSE,FALSE, FALSE)]
        return(list(cohort = oo_cohort,
                    w_c = w_c,
                    GEO_h = NA_integer_,
                    SUS_death_h = NA_integer_,
                    ZRE_ind = NA_integer_,
                    ZDI_ind = NA_integer_,
                    ZIN_ind = NA_integer_,
                    SUZ_death_ind = NA_integer_,
                    INC_h_lower = NA_integer_,
                    INC_h_upper = NA_integer_,
                    PMO_c = NA))
      }

      # calculate surviving sporangia in cohort
      w_c[indx >= GER_c_h,
          SUS_h := cumsum(calc_SUS(temp,rh))]

    # get cohort sporangia survival time
      SUS_c_h <- w_c[indx >= GER_c_h &
                       SUS_h <= 1, last(indx)]
      # After sporangia die remove PMO from cohort
      w_c[, GEO := fifelse(indx >= GER_c_h &
                             SUS_h <= 1,
                           GEO_c,
                           0)]


      # How long are zoospores Is there a zoospore release for this cohort?
      w_c[,ZooWindow := fifelse(indx >= GER_c_h & indx <= SUS_c_h,
                                TRUE,FALSE)]
      # Is there a zoospore release for this cohort?
      ## Initialise
      w_c[, REL := FALSE]
      w_c[ZooWindow==TRUE,
          REL := zsp_release(WD_h = cumsum(M_h),
                             TWD_h = cumsum(temp)/seq_along(temp))]

      # Germinated oospores making up the surviving germinated oospores from cohort
      # w_c[, GEO_h := fifelse(GER >=1, GEO_c, PMO_c)]
      # w_c[, GEO_h := fifelse(SUS_h > 1, 0, GEO_h)]
      # GEO <- w_c[J_c == oo_cohort, last(GEO_h)]

      # get p (hour of zoospore release)
      ZRE_ind <- w_c[REL == TRUE,first(indx)]

      # init SUZ; Zoospore survival
      w_c[,SUZ_h := NA_real_]

      # if zoospores don't survive return NA
      if(length(ZRE_ind) == 0){
        w_c[, c("ZRE_h",
                "INC_h",
                "ZDI_h",
                "INF_h"
        ) := list(FALSE,FALSE,FALSE,FALSE)]
       return(list(cohort = oo_cohort,
                   w_c = w_c,
                   GEO_h = GER_c_h,
                   SUS_death_h = SUS_c_h,
                   ZRE_ind = NA_integer_,
                   ZDI_ind = NA_integer_,
                   ZIN_ind = NA_integer_,
                   SUZ_death_ind = NA_integer_,
                   INC_h_lower = NA_integer_,
                   INC_h_upper = NA_integer_,
                   PMO_c = NA))
      }

      # calculate the zoospore survival
      # I don't understand this function in the paper I think it might be wrong there
      #  I have characterised it as it is described. a function that returns a number
      #  greater than one when there are no longer consecutive hours of "wetness".
      # Another this that bugs me is that the difference between "wetness" and
      # "moisture" is not defined in the paper. This needs further research
      # if the denominator is 0 an NA is returned, this should be a number greater
      # than zero to show non-consecutive wet-hours
      SUZ <- w_c[indx >= ZRE_ind,
                 (indx - ZRE_ind) /
                          fifelse(cumsum(shift(M_h, n = 1, type = "lead")) == 0,
                                  0.2,
                                  cumsum(shift(M_h, n = 1, type = "lead")))]
      w_c[indx >= ZRE_ind,
          SUZ_h := SUZ]

      # Determine zoospore release
      ## ZRE_h is the number of zoospores released at hour _h
      w_c[,ZRE_h := fifelse(SUZ_h <= 1 &
                              SUZ_h >= 0,
                           TRUE, FALSE)]

      # Determine if a zoospore dispersal has occured
      w_c[, ZDI_h := fifelse(rain >= 0.2 & ZRE_h == TRUE,
                           TRUE,
                           FALSE)]

      # is equivalent to lower-case greek delta (ZDI_delta)
      ZDI_ind <- w_c[ZDI_h == TRUE,first(indx)]

      ## EXIT if ...
      # if zoospores don't survive return NA
      if(length(ZDI_ind) == 0){
        w_c[, c("ZDI_h",
                "INC_h",
                "INF_h"
                ) := list(FALSE,FALSE,FALSE)]
        return(list(cohort = oo_cohort,
                    w_c = w_c,
                    GEO_h = GER_c_h,
                    SUS_death_h = SUS_c_h,
                    ZRE_ind = ZRE_ind,
                    ZDI_ind = NA_integer_,
                    ZIN_ind = NA_integer_,
                    SUZ_death_ind = w_c[ZRE_h == TRUE, last(indx)],
                    INC_h_lower = NA_integer_,
                    INC_h_upper = NA_integer_,
                    PMO_c = NA))
      }

      # Determine if zoospores successfully infect INF_h
      # initialise INF_h
      w_c[,INF_h := FALSE]
      w_c[indx >= ZDI_ind &
            ZRE_h == TRUE, # Infection has to occur before zoospores dry out which is calculated from ZRE_h
          INF_h := fifelse(cumsum(M_h)*(cumsum(temp)/seq_along(temp)) >= 60,
                          TRUE,FALSE)]

      # is equivalent to lowercase greek iota (ZIN_iota)
      # hour of zoospore infection
      zoo_infection_ind <- w_c[INF_h == TRUE,first(indx)]

      ## EXIT if ...
      #  zoospores don't infect return NA
      if(length(zoo_infection_ind) == 0){
        w_c[, c("INC_h",
                "INF_h") := list(FALSE,FALSE)]
        return(list(cohort = oo_cohort,
                    w_c = w_c,
                    GEO_h = GER_c_h,
                    SUS_death_h = SUS_c_h,
                    ZRE_ind = ZRE_ind,
                    ZDI_ind = ZDI_ind,
                    ZIN_ind = NA_integer_,
                    SUZ_death_ind = w_c[ZRE_h == TRUE, last(indx)],
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
                  GEO_h = GER_c_h,
                  SUS_death_h = SUS_c_h,
                  ZRE_ind = ZRE_ind,
                  ZDI_ind = ZDI_ind,
                  ZIN_ind = zoo_infection_ind,
                  SUZ_death_ind = w_c[ZRE_h == TRUE, last(indx)],
                  INC_h_lower = INC_h_lower,
                  INC_h_upper = INC_h_upper,
                  PMO_c = PMO_c))

    })


  m_viticola_out <- list(start_time = Start,
                         time_hours = w$times,
                         Hyd_t = w$HT_h,
                         PMO = w$PMO,
                         cohorts = max(oospore_cohorts),
                         w = w,
                         cohort_list = cohort_list
  )
  class(m_viticola_out) <- c("m_viticolR",class(m_viticola_out))

  return(m_viticola_out)


  # The HT is also used to calculate the length of the primary inoculum season
  #  (PIS): PIS starts and finishes when HT = 1.3 and 8.6, respectively (Fig. 3).
  # These values correspond to the time when 3% and 97% of the total oospores
  #  forming the SOD have entered the PMO stage, respectively, according to Eq. (3)
  # plot(w$times, w$PMO, type="l")
  # plot(w$times, w$HT_h, type="l")
}# end of estimate_DM_PLR




