#' Plot ribbon viticolaR model
#'
#' @usage geom_ribbon_viticolaR(mod, y = "GER", x_subset = "ZooWindow",...)
#' @aliases geom_line_viticolaR(mod, y = "GER", ...)
#' @param mod m_viticola class object, output of function estimate_DM_PI
#' @param y character, An output state variable from the estimate_DM_PI, defaults
#'  to "GER" other options include "PMO", "GEO", "SUZ_h".
#' @param x_subset a column heading in w_c of the cohort list with a logical class
#'  This will subset the ribbon.
#' @param ... other arguments to be passed to geom_line
#'
#' @return ggplot
#' @export
#'
#' @import ggplot2
#' @examples
#' v_mod <- estimate_DM_PI(w = nt_weather,
#'                         Start = as.POSIXct("2023-07-01"),
#'                         End = as.POSIXct("2023-08-30"))
#' ggplot2::ggplot() +
#'    geom_ribbon_viticolaR(v_mod)
geom_ribbon_viticolaR <- function(mod,
                     y = "GER",
                     x_subset = "ZooWindow",
                     ...){
  times <- NULL

      lapply(mod$cohort_list,function(x){
      ggplot2::geom_ribbon(
        data = x$w_c[get(x_subset) == TRUE],
        ggplot2::aes(x = times,
        ymin = 0,
        ymax = get(y),
        fill = x$cohort),...)
      })
}

#' Plot line viticolaR model
#'
#' @param mod m_viticola class object, output of function estimate_DM_PI
#' @param y character, An output state variable from the estimate_DM_PI, defaults
#'  to "GER" other options include "PMO", "GEO", "SUZ_h".
#' @param ... other arguments to be passed to geom_line
#'
#' @return ggplot layer
#' @export
#'
#' @examples
#' v_mod <- estimate_DM_PI(w = nt_weather,
#'                         Start = as.POSIXct("2023-07-01"),
#'                         End = as.POSIXct("2023-08-30"))
#' ggplot2::ggplot() +
#'    geom_line_viticolaR(v_mod)
geom_line_viticolaR <- function(mod,
                                  y = "GER",
                                  ...){
  times <- NULL

      lapply(mod$cohort_list,function(x){
        ggplot2::geom_line(
          data = x$w_c,
          ggplot2::aes(x = times,
          y = get(y),
          colour = x$cohort),...)

        })
  }
# list(scale_fill_gradient(low = "#7e3802", high = "#fcfec8"),
#      ylim(0, 1.2)))

#' Plot viticolR weather
#'
#' @param mod model output from `estimate_DM_PI()` with class 'm_viticola'
#'
#' @return plot
#' @export
#'
#' @examples
#' v_mod <- estimate_DM_PI(w = nt_weather,
#'                         Start = as.POSIXct("2023-07-01"),
#'                         End = as.POSIXct("2023-08-30"))
#' plot_weather(v_mod)
plot_weather <- function(mod, rolling_window = 4){
  if(isFALSE(inherits(mod,what = "m_viticola"))) stop("'mod' is not class 'm_viticola'.
                                                        Please use an output of 'estimate_DM_PI()'")
  w_dat <- mod$w

  factr <- floor(nrow(w_dat)/rolling_window)
  f_group <- c(rep(1:factr,each = rolling_window),rep(factr+1,nrow(w_dat) %% rolling_window))

  w_dat[,time_factor := f_group]
  w_agg <- w_dat[,list(times = median(times),
                         temp = mean(temp),
                       rh = mean(rh),
                       rain = sum(rain,na.rm = TRUE)), by = f_group]

  w_agg |>
    ggplot(aes(x = times))+
    geom_line(aes(y = temp),colour = "darkred")+
    geom_col(aes(y = rain),
                # method = "glm",
                # formula = y ~ poly(x,20),
                colour = "lightblue")+
    ylab("Temperature C and rainfall (mm)")+
    geom_line(aes(y = rh/4),colour = "darkblue",linetype = "dotdash")+
    scale_y_continuous(sec.axis = sec_axis(transform = ~./0.25,name = "Relative Humidity %"))+
    theme_minimal()+
    ggtitle(paste(unique(mod$w$station), "weather observations"))
}

