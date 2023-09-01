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
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#'
#' @examples
estimate_DM_PI <- function(w,
                           Start = "detect",
                           End = "detect",
                           SOD = 1){

  if(("epiphy.weather" %in% w) == FALSE) stop("'w' must be class epiphy.weather
                                              use 'format_weather()'")

  if(length(unique(w[,.(station)])) > 1)stop("Model does not currently support weather
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
    Yr <- year(w[1,times])
    if(w[1,times] < as.POSIXct(paste0(Yr,"-06-01 00:00:00"),tz = "UTC")){
      Start <- as.POSIXct(paste0(Yr,"-06-01 00:00:00"),tz = "UTC")
    }else{
      Start <- w[1,times]
    }
  }

  if(End == "detect"){
    End <-
      fifelse(w[.N,times] < as.POSIXct(paste0(Yr + 1,"-05-29 23:00:00"),tz = "UTC"),
              w[.N,times],
              as.POSIXct(paste0(Yr + 1,"-05-29 23:00:00"),tz = "UTC"))
  }
  # select weather data
  w <- w[times >= Start &
           times <= End]

  # add VPD column
  w[, vpd := epiphytoolR::calc_vpd(rh,temp)]

  # Leaf litter moisture sufficient for oospore maturation
  # dichotomic variable
  w[ , M_h := fcase(rain > 0,1,
                    as.numeric(vpd) == 0.45, 1,
                    as.numeric(vpd) > 0.45, 0,
                    default = 0)]

  # Calculate hydrothermal time
  w[, HT_h := calc_HT(temp,M_h)]

  # calculate rate at which dormancy breaks
  # equation 2
  w[, DOR := exp(-15.891 * exp(-0.653*(HT_h + 1)))]

  #calculate physiological maturity
  w[, PMO := MMO * DOR]

}# end of estimate_DM_PLR




