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
  if(Start == "detect"){
    Yr <- data.table::year(w$times[1])
    if(w$times[1] < as.POSIXct(paste0(Yr,"-06-01 00:00:00"),tz = "UTC")){
      Start <- as.POSIXct(paste0(Yr,"-06-01 00:00:00"),tz = "UTC")
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

  # Leaf litter moisture sufficient for oospore maturation
  # dichotomic variable
  w[, M_h := data.table::fcase(rain > 0, 1,
                               as.numeric(vpd) == 0.45,1,
                               as.numeric(vpd) > 0.45,0,
                               default = 0)]

  # Calculate cumulative hydrothermal time
  w[, HT_h := cumsum(calc_HT(temp,M_h))]

  # calculate rate at which dormancy breaks
  # equation 2
  w[, DOR := exp(-15.891 * exp(-0.653*(HT_h + 1)))]

  #calculate physiological maturity
  w[, PMO := MMO * DOR]

  return(w)


  # The HT is also used to calculate the length of the primary inoculum season
  #  (PIS): PIS starts and finishes when HT = 1.3 and 8.6, respectively (Fig. 3).
  # These values correspond to the time when 3% and 97% of the total oospores
  #  forming the SOD have entered the PMO stage, respectively, according to Eq. (3)
  # plot(w$times, w$PMO, type="l")
  # plot(w$times, w$HT_h, type="l")
}# end of estimate_DM_PLR




