#' North Tamborine formatted weather data
#'
#' This data was retreived from the portable/ temporary weather station data published
#'  by the Beureu of Meterology on behalf of the Queensland Fire and Resuce Service
#'  and modified.
#'
#' @format ## `nt_weather`
#' A data.table with 2215 rows and 18 columns:
#' \describe{
#'   \item{times}{hourly POSIXct date time}
#'   \item{temp}{mean hourly temperature in degrees celcius}
#'   \item{rh}{mean hourly relative humidity as a percentage}
#'   \item{rain}{hourly rain total in millimeters}
#'   \item{ws}{mean hourly wind speed in km/h}
#'   \item{wd}{mean hourly wind direction in degrees}
#'   \item{sd_wd}{hourly standard deviation of wind direction}
#'   \item{lon}{longitude of weather station measurment}
#'   \item{lat}{latitude of weather station measurment}
#'   \item{station}{character string with the name of the weather station}
#'   \item{YYYY}{Year of weather station measurement}
#'   \item{MM}{Month of weather station measurement}
#'   \item{DD}{Day of the month when weather station measurement was taken}
#'   \item{hh}{Hour of the day when weather station measurement was taken}
#'   \item{mm}{minute of the hour when weather station measurement was taken, Zero
#'    if the hourly mean}
#'   \item{indx}{index giving the line number}
#'   ...
#' }
#' @source <http://www.bom.gov.au/products/IDQ60801/IDQ60801.99123.shtml#other_formats>
"nt_weather"
