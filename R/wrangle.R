#' Unix timestamp to local timezone datetime
#' @description Convert a time stamp into local timezone datetime
#' @param timestamp Unix time stamp vector
#' @param long Longitude! as vector
#' @param lat   Latitude! as vector
#' @return Returns "POSIXct", "POSIXt" vector in NZ time zone
#' @export
#' @examples
#' make_nz_datetime(1555555555, -125.357, 23.556)
make_nz_datetime <- function(timestamp, long = 0, lat = 0) {
  checkmate::assert_numeric(timestamp)
  checkmate::assert_numeric(long)
  checkmate::assert_numeric(lat)

  # test only - NZ Values
  # https://stackoverflow.com/questions/59833660/
  # convert-to-local-time-zone-from-latitude-and-longitude-r
  # --------------------------------------------------------
  # timestamp = 1655112631
  # lat = -40.9006
  # long = 174.8860

  # Get local timezone baesd on lat & long
  local_tz = tz_lookup_coords(lat = lat, lon = long, method = "accurate")

  # convert to local time plus timezone format
  local_date <- timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    lubridate::with_tz(tz = local_tz)
}

