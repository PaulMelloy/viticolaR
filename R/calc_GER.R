#' Calculate hourly oospore germination
#'
#' Calculate the number of germinated oospores in an hour increment using temperature
#'  and moisture. Equation 5 of Rossi (2008)
#'
#' @return
#' @export
#'
#' @examples
calc_GER <- function(M_h, T_h){

  if(T_h <= 0) return(0)

  GER_h <- M_h/(1330.1 - 116.19*T_h + 2.6256 * (T_h^2))

  return(GER_h)
}
