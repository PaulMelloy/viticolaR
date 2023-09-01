## code to prepare `DATASET` dataset goes here
library(data.table)
library(epiphytoolR)

# read in the raw data
ntamborine <- fread("../weather_north_tamborine.csv")
# save raw data to package
usethis::use_data(ntamborine, overwrite = TRUE)

# Can't modify package data so re-allocate it
nt_weather <- ntamborine

# format times
nt_weather[,aifstime_utc := as.POSIXct(as.character(aifstime_utc),
                                       format = "%Y%m%d%H%M%S",
                                       tz = "UTC")]

# for information on what the column headers relate to
#  http://www.bom.gov.au/catalogue/Observations-XML.pdf

nt_weather <-
  format_weather(
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
    data_check = FALSE)


usethis::use_data(nt_weather, overwrite = TRUE)
