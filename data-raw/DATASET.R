## code to prepare `DATASET` dataset goes here
# library(data.table)
# library(epiphytoolR)

# if(Sys.info()["nodename"] == "rstudio") {
#   # read in latest data
#   ntamborine <- fread("~/Weather observations/NTamborine.csv")
#   fwrite("inst/extdata/weather_north_tamborine.csv")
# } else{
#   # read in the raw data
#   ntamborine <- fread("inst/extdata/weather_north_tamborine.csv")
# }
# # save raw data to package
# # Edit I don't know if it is needed
# #usethis::use_data(ntamborine, overwrite = TRUE)
#
# # Can't modify package data so re-allocate it
# nt_weather <- data.table::fread(
#   system.file("extdata",
#               "weather_north_tamborine.csv",
#               package = "viticolR"))

nt_weather[,lon := 153.1914]
nt_weather[,lat := -27.9396]

# format times
nt_weather[,aifstime_utc := as.POSIXct(as.character(aifstime_utc),
                                       format = "%Y%m%d%H%M%S",
                                       tz = "UTC")]

# for information on what the column headers relate to
#  http://www.bom.gov.au/catalogue/Observations-XML.pdf
suppressWarnings(
nt_weather <-
  epiphytoolR::format_weather(
    nt_weather,
    POSIXct_time = "aifstime_utc",
    time_zone = "UTC",
    temp = "air_temp",
    rain = "rain_ten",
    rh = "rel_hum",
    ws = "wind_spd_kmh",
    wd = "wind_dir_deg",
    station = "name",
    lon = "lon",
    lat = "lat",
    data_check = FALSE))

# set the width of the rolling window, this will impact the smoothing of the
#  imputation
rolling_window <- 60
# set the index in the table, this is needed for the rolling apply function
nt_weather[,indx := .I]

## check how many NAs are in the temperature column
# nt_weather[is.na(temp),indx]

# Rolling apply using the impute_fill function on temperature variable
nt_weather[, tm_imp := round(data.table::frollapply(
  indx,
  n = rolling_window,
  fill = NA_real_,
  FUN = epiphytoolR::impute_fill,
  FUN_n = rolling_window,
  times = times,
  var = temp,
  align = "center"
),3)]


# # visualise the fit of the fill
# plot(w$temp[1050:1600], type = "l")
# lines(w$tm_imp[1050:1600], type = "l", col = "blue")
# abline(v = seq(0,550, by = 24))

# set the NAs in temperature with the estimated temperature
nt_weather[is.na(temp), temp:= tm_imp]

# # Check how many NAs remain
# nt_weather[is.na(temp),indx]

# We could widen the rolling_window, so we only need to impute once or we can
#  run the function twice.
# we can run the impute_fill function over the same data to fill in the rest
dif <- 20
while(dif > 0) {
  #get nas
  na_s <- length(nt_weather[is.na(temp), indx])
  nt_weather[, tm_imp := round(
    data.table::frollapply(
      indx,
      n = rolling_window,
      fill = NA_real_,
      FUN = epiphytoolR::impute_fill,
      FUN_n = rolling_window,
      times = times,
      var = temp,
      align = "center"
    ),
    3
  )]

  # set the NAs in temperature with the estimated temperature
  nt_weather[is.na(temp), temp := tm_imp]


  # Check how many NAs remain
  dif <- na_s - length(nt_weather[is.na(temp), indx])
  # cat("Dif remaining: ", dif,"\n")
}
# # visualise the fit of the fill
# plot(nt_weather$temp[2200:2243], type = "l")
# lines(nt_weather$tm_imp[2200:2243], type = "l", col = "blue")
# abline(v = seq(0,43, by = 24))


# Impute Relative humidity _____________________
dif <- length(nt_weather[is.na(rh), indx])
while(dif > 0) {
  #get nas
  na_s <- length(nt_weather[is.na(rh), indx])
  nt_weather[, rh_imp := round(
    data.table::frollapply(
      indx,
      n = rolling_window,
      fill = NA_real_,
      FUN = epiphytoolR::impute_fill,
      FUN_n = rolling_window,
      times = times,
      var = rh,
      align = "center"
    ),
    3
  )]
  # set the NAs in temperature with the estimated temperature
  nt_weather[is.na(rh), rh := rh_imp]
  # Check how many NAs remain
  dif <- na_s - length(nt_weather[is.na(rh), indx])
  ## Report
  # cat("Dif remaining: ", dif,"\n")
}



# # check how many NA entries remain and trim them
# nt_weather[, .(temp_nas = sum(is.na(temp)),
#       rh_nas = sum(is.na(rh)))]
# dim(nt_weather)
# Remove lines with NAs
nt_weather <- nt_weather[1:(which(is.na(temp))[1]-1)]
# dim(nt_weather)

# fill rain NAs with 0
nt_weather[is.na(rain), rain := 0]

# remove imp columns
nt_weather[,c("tm_imp", "rh_imp"):= list(NULL,NULL)]

# store and overwrite
usethis::use_data(nt_weather, overwrite = TRUE)
