% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg_methods.R
\name{plot_weather}
\alias{plot_weather}
\title{Plot viticolR weather}
\usage{
plot_weather(mod, rolling_window = 4, date_min, date_max)
}
\arguments{
\item{mod}{model output from \code{estimate_DM_PI()} with class 'm_viticola'}

\item{rolling_window}{summarise hourly weather data into rolling average
(temperature and relative humidity), cumulative rainfall and median \code{times}}

\item{date_min}{as.posix date minimum for x-axis of plot}

\item{date_max}{as.posix date maximum for x-axis of plot}
}
\value{
ggplot of weather
}
\description{
Plot viticolR weather
}
\examples{
v_mod <- estimate_DM_PI(w = nt_weather,
                        Start = as.POSIXct("2023-07-01"),
                        End = as.POSIXct("2023-08-30"))
plot_weather(v_mod)
}
