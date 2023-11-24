#' Calculate sporangia survival
#'
#' @param T_h numeric, hourly temperature mean
#' @param RH_h numeric, hourly relative humidity mean
#'
#' @return numeric, a non-dimensional number to indicate the length of sporangia
#'  survival
calc_SUS <- function(T_h,RH_h){
  RH_h <- data.table::fifelse(RH_h == 100,
                              99.9,
                              RH_h,
                              na = NA)
  SUS <- 1/(
    24*(5.67 - 0.47 * (T_h *(1 - (RH_h/100))) +
          0.01 *(T_h * (1 - (RH_h/100)))^2))
  return(SUS)
}
