#' Calculate physiologically
#'
#' @param hh hours after midnight on January first (new years)
#' @param MMO Mophologically Mature Oospores
#' @param DOR Progress of dormancy converting mophologically mature oospores (MMO)
#'  to Physiologically Mature Oospores (PMO)
#'
#' @return
#' @export
#'
#' @examples
calc_PMO <- function(hh,MMO, DOR){
  MMO * calc_DOR(hh)
}
