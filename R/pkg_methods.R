#' Plot ribbon viticolaR model
#'
#' @usage geom_ribbon_viticolaR(mod, y = "GER", x_subset = "ZooWindow",...)
#' @aliases geom_line_viticolaR(mod, y = "GER", ...)
#' @param mod m_viticola class object, output of function estimate_DM_PI
#' @param y character, An output state variable from the estimate_DM_PI, defaults
#'  to "GER" (germinating oospores), other options include "PMO" (Physiological
#'  mature oospores), "GEO" (Germinated oospores), "SUS_h" (survival of sporangia),
#'  "SUZ_h" (Survival of zoospores).
#' @param x_subset a column heading in w_c of the cohort list with a logical class
#'  This will subset the ribbon, options include "ZRE_h" (Zoospore release),
#'  "ZDI_h" (Zoospore dispersal hours), "REL" and "ZooWindow" (zoospores on
#'  leaves).
#' @param ... other arguments to be passed to geom_ribbon
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
#'
#' ggplot2::ggplot() +
#'    geom_ribbon_viticolaR(v_mod,
#'                          y = "SUZ_h",
#'                          x_subset = "ZRE_h")
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
#' @param rolling_window summarise hourly weather data into rolling average
#'  (temperature and relative humidity), cumulative rainfall and median `times`
#' @param date_min as.posix date minimum for x-axis of plot
#' @param date_max as.posix date maximum for x-axis of plot
#'
#' @return ggplot of weather
#' @export
#'
#' @examples
#' v_mod <- estimate_DM_PI(w = nt_weather,
#'                         Start = as.POSIXct("2023-07-01"),
#'                         End = as.POSIXct("2023-08-30"))
#' plot_weather(v_mod)
plot_weather <- function(mod,
                         rolling_window = 4,
                         date_min,
                         date_max){
  # conciliate missing globals
  time_factor <- times <- temp <- rh <- rain <- NULL

  if(isFALSE(inherits(mod,what = "m_viticola"))) stop("'mod' is not class 'm_viticola'.
                                                        Please use an output of 'estimate_DM_PI()'")
  if(missing(date_min)){
    if(missing(date_max)){
      w_dat <- mod$w
      }else{
        w_dat <- mod$w[times <= as.POSIXct(date_max)]
      }
  }else{
    if(missing(date_max)){
      w_dat <- mod$w[times >= as.POSIXct(date_min)]
    }else{
      w_dat <- mod$w[times >= as.POSIXct(date_min) &
                       times <= as.POSIXct(date_max),]
  }}


  # get number of groups to summarise
  factr <- floor(nrow(w_dat)/rolling_window)
  # moderate leftover lines into last group
  f_group <- c(rep(1:factr,each = rolling_window),rep(factr+1,nrow(w_dat) %% rolling_window))

  w_dat[,time_factor := f_group]
  w_agg <- w_dat[,list(times = stats::median(times),
                         temp = mean(temp),
                       rh = mean(rh),
                       rain = sum(rain,na.rm = TRUE)), by = f_group]

  w_agg |>
    ggplot(aes(x = times))+
    geom_line(aes(y = temp),colour = "darkred")+
    geom_col(aes(y = rain),
                colour = "lightblue")+
    ylab("Temperature C (red) and \nrainfall (mm)(bars)")+
    geom_line(aes(y = rh/4),colour = "darkblue",linetype = "dotdash")+
    scale_y_continuous(sec.axis = sec_axis(transform = ~./0.25,name = "Relative Humidity %"))+
    theme_minimal()+
    ggtitle(paste(unique(mod$w$station), "weather observations"))
}

#' Summary of m_viticola class object
#'
#' @param object m_viticola class object, output of function estimate_DM_PI
#' @param ... other arguments to be passed to summary, currently not used.
#'
#' @returns a summary of the model run time, weather station, germinated oospore
#' @export
#'
#' @examples
#' mod1 <- estimate_DM_PI(w = nt_weather,
#'                        Start = as.Date("2023-07-01"),
#'                        End = as.Date("2023-08-30"))
#' summary(mod1)
summary.m_viticola <- function(object, ...){

  # Define globals
  primary_infection_stage <- NULL

  PI_dates <- get_PI_dates(object)

  cat("ViticolaR model summary\n")
  cat("  Model run time: Start : ",as.character(object$time_hours[1]),"\n")
  cat("                  End   : ",as.character(object$time_hours[length(object$time_hours)]),"\n")
  cat("                  Days  : ",round(length(object$w$times)/24,1),"\n")
  cat("\n")
  cat("  Weather station : ",unique(object$w$station)," \n")
  cat("  Germinated oospore cohorts :",object$cohorts, "\n")
  cat("\n")
  cat("  Proportion of physiologically mature oospores (PMO) germinated this season : :",
      object$PMO[length(object$PMO)], "\n")
  cat("  Estimated sporangia production dates : ",
      paste(stats::na.exclude(PI_dates[primary_infection_stage == "GEO_h", unique(as.Date(hour))]),
            collapse = "\n                                          "), "\n")
  cat("  Estimated Zoospore dispersal dates : ",
      paste(stats::na.exclude(PI_dates[primary_infection_stage == "ZDI_ind", unique(as.Date(hour))]),
            collapse = "\n                                        "), "\n")
  lwer <- stats::na.exclude(PI_dates[primary_infection_stage == "INC_h_lower", unique(as.Date(hour))])
  uper <- stats::na.exclude(PI_dates[primary_infection_stage == "INC_h_upper", unique(as.Date(hour))])
  symp_range <- paste(lwer,uper, sep = " - ")
  cat("  Estimated dates with successful Zoospore infection : ",
      paste(stats::na.exclude(PI_dates[primary_infection_stage == "ZIN_ind", unique(as.Date(hour))]),
            collapse = "\n                                        "),
      "\n")
  cat("  Estimated dates range for symptom expression : ",
      paste(symp_range,
            collapse = "                                                        \n"),
      "\n")

}
