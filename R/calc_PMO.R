#' Calculate physiologically
#'
#' @param hh hours after midnight on July 1st (Southern Hemisphere), January first
#'  (Northern hemisphere, new years),
#' @param MMO Morphologically Mature Oospores at the start of the year
#' @param DOR Progress of dormancy converting mophologically mature oospores (MMO)
#'  to Physiologically Mature Oospores (PMO)
#'
#' @return
#' @export
#'
#' @examples
#' calc_PMO(123,1,0.002)
calc_PMO <- function(hh,MMO, DOR){
  MMO * calc_DOR(hh)
}
