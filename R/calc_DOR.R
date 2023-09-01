#' Title
#'
#' @param Tm
#' @param vpd
#' @param rain
#'
#' @return
#' @export
#'
#' @examples
calc_DOR <- function(Tm_h, vpd_h, rain_h){

  if(rain > 0 | vpd == 4.5){
    M_h <- 1
  }else{
    if(rain == 0 & vpd > 4.5){
      M_h <- 0
    }
  }





  DOR_h <- exp(-15.891*exp(-0.653*(HT_h + 1)))
}
