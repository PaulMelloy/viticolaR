#' Calculate the cohort size of physiologically mature oospores
#'
#' @param j rain event number triggering oospore germination
#' @param epslon hour at which the germination is triggered
#' @param DOR_h rate at which oospores mature at hour h
#'
#' @return the PMO
#'
#' @noRd
calc_PMO_cohort <- function(j, epslon,DOR_h){
  # mean(DOR_h[j],DOR_h[j-1])

  # # Index oospore cohort
  # w[, J_cohort := 0] # initiate the variable
  # for(i in seq_along(w$J_cohort)){
  #   if(i == 1) next
  #   c1 <- fifelse(w[i-1,rain] < 0.2 & w[i,rain] >= 0.2,
  #                 w[i,J_cohort] +1,
  #                 w[i,J_cohort])
  #   w[i:.N, J_cohort := c1]
  #
  # }
}
