#' Calculate leaf litter Moisture
#'
#' @param Tm numeric, hourly mean Temperature in degrees
#' @param vpd numeric, hourly mean vapour pressure defiecit in hPa
#' @param rain numeric, hourly rainfall mean in mm
#'
#' @return
#'
#' @examples
calc_DOR <- function(Tm_h, vpd_h, rain_h){
  rain <- vpd <- HT_h <- NULL

  if(rain > 0 | vpd == 4.5){
    M_h <- 1
  }else{
    if(rain == 0 & vpd > 4.5){
      M_h <- 0
    }
  }





  DOR_h <- exp(-15.891*exp(-0.653*(HT_h + 1)))
}
