calc_SUS <- function(T_h,RH_h){
  SUS <- 1/(
    24*(5.67 * 0.47 *
          (T_h *(1 - RH_h/100)) +
          0.01 *(T_h * (1 - RH_h/100))^2))
}
