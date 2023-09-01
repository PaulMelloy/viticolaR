calc_DOR <- function(hh, Tm, vpd, rain){

  if(rain > 0 | vpd == 4.5){
    M_h <- 1
  }else{
    if(rain == 0 & vpd > 4.5){
      M_h <- 0
    }
  }

  # The HT (hydrothermal time) is also used to calculate the length of the primary
  #  inoculum season (PIS): PIS starts and finishes when HT = 1.3 and 8.6,
  #  respectively (Fig. 3).
  # These values correspond to the time when 3% and 97% of the total oospores
  #  forming the SOD have entered the PMO stage, respectively, according to Eq. (3)


  #Calculate Hydrothermal time
  HT_h <- M_h/(1330.1 - 116.19 * T_h + 2.6256 * (T_h^2))

  DOR_h <- exp(-15.891*exp(-0.653*(HT_h + 1)))
}
