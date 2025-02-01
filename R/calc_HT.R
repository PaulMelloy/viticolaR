#' Calculate Hydrothermal Time
#'
#' Used to determine what the rate at which oospores mature. Described as equation
#'  three in \insertRef{rossi_mechanistic_2008}{viticolR}
#'
#' @param Tm_h hourly temperature in degrees
#' @param M_h logical, Leaf litter moisture sufficient for maturation
#'
#' @return numeric, Hydrothermal time
#' @export
#' @references
#' \insertAllCited{}
#'
#' @examples
#' calc_HT(24,1)
calc_HT <- function(Tm_h, M_h){

  # The HT (hydrothermal time) is also used to calculate the length of the primary
  #  inoculum season (PIS): PIS starts and finishes when HT = 1.3 and 8.6,
  #  respectively (Fig. 3).
  # These values correspond to the time when 3% and 97% of the total oospores
  #  forming the SOD have entered the PMO stage, respectively, according to Eq. (3)
  if(length(Tm_h) != length(M_h)) stop("Tm_h and M_h must have the same length")

  df1 <- data.frame(Tm = Tm_h,
                    M = M_h)

  #Calculate Hydrothermal time
  HT <- apply(df1,1,function(x){
    if(as.numeric(x["Tm"]) <=0) return(0)
    as.numeric(x["M"])/
      (1330.1 - 116.19 * as.numeric(x["Tm"]) + 2.6256 * (as.numeric(x["Tm"])^2))
  })

  return(unlist(HT))
}
