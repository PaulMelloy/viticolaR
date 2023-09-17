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
  dt1 <- data.table(M_h = M_h,
                    T_h = T_h)

  GER_out <-
    apply(dt1,1,function(x){
      Tm <- as.numeric(x["T_h"])
      if(Tm <= 0) return(0)

      GER_h <- as.numeric(x["M_h"])/
        (1330.1 - 116.19* Tm + 2.6256 * (Tm^2))

      return(GER_h)
  })
  return(GER_out)
}
