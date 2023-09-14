#' Calculate leaf litter Moisture
#'
#' @param Tm numeric, hourly mean Temperature in degrees
#' @param vpd numeric, hourly mean vapour pressure deficit in hPa
#' @param rain numeric, hourly rainfall mean in mm
#'
#' @return
#'
#' @examples
#' calc_DOR(Tm_h = 22,
#'          vpd_h = 0.05,
#'          rain_h = 0)
calc_DOR <- function(Tm_h, vpd_h, rain_h){
  rain <- vpd <- HT_h <- NULL

  if(rain_h > 0 | vpd_h <= 4.5){
    M_h <- 1
  }else{
    if(rain_h == 0 & vpd_h > 4.5){
      M_h <- 0
    }

    return(M_h)
  }





  DOR_h <- exp(-15.891*exp(-0.653*(HT_h + 1)))
}
