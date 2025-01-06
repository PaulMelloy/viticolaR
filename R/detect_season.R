#' Detect latest season
#'
#' Returns the most recent year for which weather data contains weather
#'  observations
#'
#' @param w hourly weather data.table, class epiphy.weather.
#'
#' @returns integer
#' @noRd
detect_season <- function(w){
  if("epiphy.weather" %in% class(w) == FALSE){
    stop("detect season needs a formatted weather file of class 'epiphy.weather'",
         "  See 'epiphytoolR::format_weather()'")
  }

  times <- NULL

  # get all the years in data
  years <- w[,unique(data.table::year(times))]

  latest_season <-
    data.table::fifelse(utils::tail(w$times,n = 1L) > as.POSIXct(paste0(max(years),"-07-15"),tz = "UTC"),
                        yes = max(years),
                        no = max(years)-1)


    if(w$times[1] < as.POSIXct(paste0(latest_season,"-07-10"),tz = "UTC")){
      earliest_season <- min(years)
    }else{
      stop(unique(w$station), "weather data does not contain observations from
      within most recent grape growing season")
    }

   return(latest_season)

}
