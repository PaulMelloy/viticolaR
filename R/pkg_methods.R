#' Plot viticolaR model
#'
#' @param mod m_viticola class object, output of function estimate_DM_PI
#' @param y character, An output state variable from the estimate_DM_PI, defaults
#'  to "GER" other options include "PMO", "GEO", "SUZ_h".
#' @param ... other arguments to be passed to geom_line
#'
#' @return ggplot
#' @export
#'
#' @examples
#' @import ggplot2
#' v_mod <- estimate_DM_PI(w = nt_weather,
#'                         Start = as.POSIXct("2023-07-01"),
#'                         End = as.POSIXct("2023-08-30"))
#' ggplot() +
#'    geom_viticolaR(v_mod)
geom_ribbon_viticolaR <- function(mod,
                     y = "GER",
                     x_subset = "ZooWindow",
                     pallet_set = "Set1",...){

      lapply(mod$cohort_list,function(x){
      geom_ribbon(data = x$w_c[get(x_subset) == TRUE],
                  aes(x = times,
                      ymin = 0,
                      ymax = get(y),
                      fill = x$cohort),...)
      })
  }
geom_line_viticolaR <- function(mod,
                                  y = "GER",
                                  x_subset = "ZooWindow",
                                  pallet_set = "Set1",...){
      lapply(mod$cohort_list,function(x){
      geom_line(data = x$w_c,
                aes(x = times,
                    y = get(y),
                    colour = x$cohort),...)

        })
  }
# list(scale_fill_gradient(low = "#7e3802", high = "#fcfec8"),
#      ylim(0, 1.2)))
