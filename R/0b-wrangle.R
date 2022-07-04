#' Unix timestamp to local timezone datetime
#'
#' @description Convert time stamp to local timezone datetime
#'
#' @param timestamp Unix time stamp vector
#' @param long Longitude as vector
#' @param lat   Latitude as vector
#'
#' @return Returns "POSIXct", "POSIXt" vector in local time zone
#' @examples
#' get_local_time(1555555555, -125.357, 23.556)
#'
#' @export
get_local_time <- function(timestamp, lat, long) {
  # checkmate::assert_numeric(timestamp)
  # checkmate::assert_numeric(lat)
  # checkmate::assert_numeric(long)

# test NZ Values :
  # https://stackoverflow.com/questions/59833660/
  # convert-to-local-time-zone-from-latitude-and-longitude-r
  # --------------------------------------------------------
  # timestamp = 1655112631
  # lat = -40.9006
  # long = 174.8860


# Real Code :
  # Get local timezone baesd on lat & long
  local_tz = lutz::tz_lookup_coords(lat = lat, lon = long, method = "fast", warn = FALSE) # not accurate

  # convert to local time plus timezone format
  local_date <- timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    # lubridate::with_tz(tz = local_tz)
    lubridate::with_tz(tz = "Etc/GMT")
}



#' Print a formatted from-to sentence
#'
#' @description Takes 2 time stamps and prints a formatted from-to sentence.
#'
#' @param from_timetamp,to_timestamp ts - begin and end of the report.
#'
#' @return Character string
#'
#' @examples
#' report_period(1555555555, 1666666666)
#'
#' @export
report_period <- function(from_timestamp, to_timestamp) {

  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)

  fromTime <- from_timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    format("%b %d")

  toTime <- to_timestamp %>%
    as.POSIXct(origin = "1970-01-01") %>%
    format('%b %d, %Y')

  return(paste("from", fromTime, "to", toTime))
}



#' wrangle_devices
#'
#' @description Wrangle devices data.\cr
#' * Replacing missing parameters with 'unknown' \cr
#' * Create room_type { living room / bedroom } \cr
#' * Fill in City and Country with best guess (to join with weather) \cr
#'
#' @param filtered_devices A data.frame or path to file
#' @param data_type_filter List of date_types to include
#' @param na.strings List of things to ignore, for fread().
#' @param integer_columns Columns with integer type.
#' @param double_columns Columns with double type.
#' @param character_columns Columns with character type.
#'
#' @return Devices Data Frame
#'
#' @importFrom readr col_character col_integer col_double
#'
#' @examples
#' wrangle_devices(test_params("monkey_a")$dev)
#'
#' @export
wrangle_devices <- function(
            filtered_devices,
            data_type_filter = c("CD", "TH", "PM", "FO", "CL"), # not really used.
            na.strings = c("",NA),
            integer_columns = c("floor"),
            double_columns = c("long", "lat"),
            character_columns =
              c("device_owner",
                "device_id",
                "data_type",
                "comms_type",
                "locationCode",
                "addr",
                "suburb",
                "city",
                "country",
                "room",
                "hhi",
                "cluster",
                "privacy",
                "risk",
                "buildingType",
                "tenure",
                "height",
                "direction",
                "school",
                "building",
                "classRoom",
                "deployment_id")) {
  # checkmate::assert_class(x = filtered_devices, classes = c("data.frame", "character"))
  # filtered_devices <-  paste0("inst/dummy-data/", params$filtered_devices)

  if (checkmate::test_character(filtered_devices)) {
    filtered_devices <-
      data.table::fread(
        filtered_devices,
        na.strings = na.strings,
        colClasses = list(character = character_columns,
                          numeric = double_columns,
                          integer = integer_columns)
      )
  }

  ## Get most common City & Country for weather data joining.
    # tryCatch (
    #   expr = {
        common_city    <- tail(names(sort(table(filtered_devices$city))), 1)
        common_country <- tail(names(sort(table(filtered_devices$country))), 1)
  #     },
  #     error = function(e){
  #       common_city     <- "unknown"
  #       common_country  <- "unknown"
  #     },
  # )

  wrangled_devices_ <-
    filtered_devices %>%

    # Filter Sensor Node
    dplyr::filter((data_type %in% data_type_filter)) %>%

    # replace missing values with 'unknown'
    dplyr::mutate(tenure       = tidyr::replace_na(tenure,       "unknown")) %>%
    dplyr::mutate(buildingType = tidyr::replace_na(buildingType, "unknown")) %>%
    dplyr::mutate(height       = tidyr::replace_na(height,       "unknown")) %>%
    dplyr::mutate(direction    = tidyr::replace_na(direction,    "unknown")) %>%
    dplyr::mutate(room         = tidyr::replace_na(room,         "unknown")) %>%
    dplyr::mutate(addr         = tidyr::replace_na(addr,         "Unknown")) %>%
    dplyr::mutate(hhi          = tidyr::replace_na(hhi,          "Unknown")) %>%
    dplyr::mutate(hhi          = stringi::stri_replace_first_fixed(hhi, "Not Listed", "Unknown") ) %>%

    dplyr::mutate(city         = tidyr::replace_na(city,        common_city)) %>%
    dplyr::mutate(country      = tidyr::replace_na(country,  common_country)) %>%

    # Mutate a room type for bedroom / living space division (default living)
    tibble::add_column(room_type = "living", .after = "room") %>%
    dplyr::mutate(room_type = dplyr::case_when(
      grepl("bedroom", tolower(room), fixed = TRUE) ~ "bedroom"
    ))

  if(nrow(wrangled_devices_) == 0) {
    stop("Oh no. Wrangled devcies is zero length.")
  }

  return(wrangled_devices_)
}



#' wrangle_weather
#'
#' @description Wrangle weather data.\cr
#' * Round ts and add hour, date, month, year (to join with obs) \cr
#' * Rename city_name -> city \cr
#' * Rename to outdoor_hum & outdoor_temp \cr
#'
#' @param weather data.frame or path to file
#'
#' @return Weather Data Frame
#'
#' @examples
#' wrangle_weather(filtered_weather = test_params("monkey_a")$weather)
#'
#' @export
wrangle_weather <- function(filtered_weather) {
  # checkmate::assert_choice(x = class(filtered_weather), choices = c("data.frame", "character"))


  if (checkmate::test_character(filtered_weather)) {
    wrangled_weather <- readr::read_csv(filtered_weather, col_types = readr::cols())
  } else {
    wrangled_weather <- filtered_weather
  }

  wrangled_weather %>%
    janitor::clean_names() %>%
    dplyr::filter(!is.na(dt)) %>%
    dplyr::mutate(city = as.character(city_name)) %>% ## prevents crash in hourly data, when empty data set

    ## extract hour, date, month, year for joining data set
    dplyr::mutate(rounded_time = 3600 * round(dt/3600) + 60) %>% # 1 minute past hour as dt is irregular.
    dplyr::mutate(time_local_iso = as.POSIXct(rounded_time, origin="1970-01-01")) %>%
    dplyr::relocate(c(rounded_time, time_local_iso, dt, dt_iso), .before = "timezone") %>%

    # Get h, d, m, Y.
    dplyr::mutate(hour  = format(as.POSIXct(time_local_iso, format = "%H:%M"),    "%H")) %>%
    dplyr::mutate(date  = format(as.POSIXct(time_local_iso, format = "%Y-%m-%d"), "%d")) %>%
    dplyr::mutate(month = format(as.POSIXct(time_local_iso, format = "%Y-%m-%d"), "%m")) %>%
    dplyr::mutate(year  = format(as.POSIXct(time_local_iso, format = "%Y-%m-%d"), "%Y")) %>%
    dplyr::relocate(c(hour, date, month, year), .before = "timezone") %>%

    ## give better names
    dplyr::mutate(outdoor_hum  = as.numeric(humidity)) %>%
    dplyr::mutate(outdoor_temp  = as.numeric(temp)) %>%

    # Clear out the crap!
    dplyr::select(
      -humidity,
      -temp,
      -sea_level,
      -grnd_level,
      -rain_3h,
      -snow_3h,
      -dt,
      -dt_iso,
      -rounded_time,
      -time_local_iso,
      -timezone,
      -lon,
      -lat
      )


    # # I want to move away from hourly averages - it is a terrible idea.
    # # long_form
    # dplyr::group_by(city, h) %>%
    # # dplyr::summarise( mean_t = mean(as.double(temp), na.rm = TRUE) ) %>%  # just temp
    # dplyr::summarise_at(dplyr::vars("outdoor_hum", "outdoor_temp"), mean, na.rm = TRUE)    # add several values!
}



#' wrangle_interventions and join with obs
#'
#' @description Wrangle intervention data\cr
#' * Expects obs df (joined with devices & weather) \cr
#' * To show the effect of an intervention, only monitors which ultimately receive
#' that intervention are included - over a fixed time period. \cr
#'   * data pre-interv 'draft-proof' flagged 'pre-draft-proof' \cr
#'   * data post-interv 'draft-proof' flagged 'post-draft-proof' \cr
#' * Comparing devices with an intervention to those without is apples & oranges. \cr
#' * Ideally, we want a volume of data: half pre ; half post... \cr
#'
#' @param filtered_interv data.frame or path to file
#' @param obs Wrangled observations df as output by `wrangle_obs`
#'
#' @return Observation Data Frame Joined with Weather & Devices & intervention data
#'
#' @examples
#' obs <- wrangle_interv(
#'   test_params("monkey_a")$interv,
#'   obs = obs
#' )
#' @export
wrangle_interv <- function(filtered_interv, obs) {
  ## check & read
  checkmate::assert_data_frame(obs)
  if (checkmate::test_character(filtered_interv)) {
    interv <- readr::read_csv(filtered_interv, col_types = "cicic")
  } else {
    interv <- filtered_interv
  }

  ## go through each interv and append flags to data
  # interv field = 'interv 1-interv 2-another interv' (NB spaces & separator)


  ## how to vis this in a chart? one line per device with intervs on x axis


}



#' wrangle_observations
#'
#' @description Wrangle measurement data\cr
#' * Filter & dedup (not actually necessary) \cr
#' * Join with devices - remove addr & email \cr
#' * Fill in missing lat / long - Gen local time\cr
#' * Add hour, date, month, year (to join with weather) \cr
#'
#' @param filtered_obs data.frame or path to file
#' @param devices Wrangled devices df as output by `wrangle_devices`
#' @param weather Wrangled outdoor weather df as output by `wrangle_weather`
#'
#' @return Obs Data Frame Joined with Weather & Devices
#'
#' @examples
#' wrangled_devices <- wrangle_devices(test_params("monkey_a")$dev)
#' wrangled_weather <- wrangle_weather(test_params("monkey_a")$weather)
#' wrangle_observations(
#'   filtered_obs = test_params("monkey_a")$obs,
#'   devices = wrangled_devices,
#'   weather = wrangled_weather
#' )
#'
#' @export
wrangle_observations <-
  function(filtered_obs, devices, weather) {

  # checkmate::assert_choice(x = class(filtered_obs), choices = c("data.frame", "character"))
    checkmate::assert_data_frame(devices)

    if (checkmate::test_character(filtered_obs)) {
      wrangled_obs <-
        readr::read_csv(filtered_obs, col_types = "ciiicd")
    } else {
      wrangled_obs <- filtered_obs
    }


    wrangled_obs <- wrangled_obs %>%
      ## only include devices present in device array
      dplyr::filter(device_id %in% devices$device_id) %>%
      ## Remove duplicate rows
      dplyr::distinct() %>%
      ## Join device data
      dplyr::left_join(
        dplyr::select( # remove addr & email addr!
          devices,
          -addr,
          -device_owner
        ),
        by = "device_id"
      ) %>%

      ## Tidy things up if you want
      # dplyr::relocate(hhi) %>%                                 # to front
      # dplyr::relocate(c(tenure, room), .before = "ts") %>%     # tidy up


      ## add default lat & long to devices missing
      # use timezone to get Local time (based on average location!)
      dplyr::mutate(lat  = tidyr::replace_na(lat,  mean(lat,na.rm=TRUE))) %>%
      dplyr::mutate(long = tidyr::replace_na(long, mean(long, na.rm=TRUE))) %>%

      ## datetime: nzst
      tibble::add_column(local_time = "", .after = "ts")


    wrangled_obs %>%
      dplyr::mutate(local_time = get_local_time(as.numeric(ts), lat, long)) %>%

      ## extract hour, date, month, year for joining data set
      dplyr::mutate(hour  = format(as.POSIXct(local_time, format = "%H:%M"),    "%H")) %>%
      dplyr::mutate(date  = format(as.POSIXct(local_time, format = "%Y-%m-%d"), "%d")) %>%
      dplyr::mutate(month = format(as.POSIXct(local_time, format = "%Y-%m-%d"), "%m")) %>%
      dplyr::mutate(year  = format(as.POSIXct(local_time, format = "%Y-%m-%d"), "%Y")) %>%
      dplyr::relocate(c(hour, date, month, year), .after = "local_time") %>%

      dplyr::left_join(weather, by = c("hour", "date", "month", "year", "city"))
}



#' get_data_volume
#'
#' @description Check data volume and flag to exclude devices with < 10% data
#'
#' @param observations from `wrangle_observations`
#' @param from_timetamp,to_timestamp timestamps for beginning & end of the report
#'
#' @returns compact data frame with : \cr
#' * device_id,  \cr
#' * n() records total,  \cr
#' * r % records present,  \cr
#' * exclude. \cr
#'
#' @examples
#' data_volume <- get_data_volume(
#'   observations = wrangled_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp
#'   )
#'
#' @export
get_data_volume <- function(observations, from_timestamp, to_timestamp) {

  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)
  checkmate::assert_data_frame(observations)


  ## How many records should there be?
  interval <- 15 * 60
  timespan <- to_timestamp - from_timestamp
  fullvol  <- (timespan / interval) + 1


  ## How many are there? Exclude those with < 10%
  observations %>%
    dplyr::group_by(device_id, hhi, tenure) %>%
    dplyr::summarise(
      n = dplyr::n(),
      r = dplyr::n()/fullvol*100 %>% round(., digits = 1),
      end = max(local_time),
      begin = min(local_time),
      exclude = dplyr::if_else(r >= 10, 0, 1)) %>%
    dplyr::arrange(r)
}



#' get_devices_for_map
#'
#' @description Filter the wrangled devices dataset to keep only devices with sufficient
#' volume for the map. This step has to go before exclusions so disconnected devices show.
#'
#' @param devices a wrangled devices data frame as output by `wrangle_devices`
#' @param data_volume the data volume data frame as output by `get_data_volume`
#'
#' @return the same dataframe but filtered for appropirate volumes of data
#'
#' @export
get_devices_for_map <- function(devices, data_volume) {

  checkmate::assert_data_frame(devices)
  checkmate::assert_data_frame(data_volume)

  # ## Test Code
  # devices        = wrangled_devices

  devices %>%
    dplyr::mutate(connected = device_id %in% data_volume$device_id) %>%
    dplyr::select(c(device_id, long, lat, tenure, connected, city)) %>%
    dplyr::mutate(has_coords = case_when (
      is.na(long) ~ FALSE,
      is.na(lat)  ~ FALSE,
      long == 0   ~ FALSE,
      lat  == 0   ~ FALSE,
      TRUE ~ TRUE
    ))
}



#' remove_excluded_devices
#'
#' @description Remove rows from `target_data` if they are flagged to `exclude`
#' in the data_volume calculation - works for dev, interv and obs data frames!
#'
#' @param target_data data.frame, either the devices or observations tables
#' @param data_volume the data volume data frame as output by `get_data_volume`
#'
#' @return the same dataframe but filtered to exclude flagged cases
#'
#' @export
remove_excluded_devices <- function(target_data, data_volume) {
  checkmate::assert_data_frame(target_data)
  checkmate::assert_data_frame(data_volume)
  dplyr::filter(target_data, device_id %in% data_volume$device_id[data_volume$exclude == 0])
}



#' make_polygon_area
#'
#' @description Make little coloured area in plot
#'
#' @param observations data.frame, the output of `wrange_observations`
#' @param target_variable This can be {"hum", "temp", "co2" ...}
#' @param severity This can be {1, 2, 3} which band it defines.
#'
#' @return a tibble dataframe of polygon data

#' @export
make_polygon_area <- function(observations, target_variable, severity) {

  checkmate::assert_data_frame(observations)
  checkmate::test_character(target_variable)
  checkmate::assert_number(severity)

  # len <- length(observations)
  corner = c("bottom-left", "top-left", "top-right", "bottom-right")
  local_time = c(min(observations$local_time), min(observations$local_time), max(observations$local_time), max(observations$local_time))

  # define limits from least severe.
  temp_limits  <- c(18,   16,   12,     0)
  hum_limits   <- c(70,   80,   90,   100)
  co2_limits   <- c(850, 1200, 1500, 3000)
  pm1_limits   <- c(10, 20, 30, 40)
  pm2_5_limits <- c(10, 20, 30, 40)
  pm10_limits  <- c(10, 20, 30, 40)
  hcho_limits  <- c(10, 20, 30, 40)
  nox_limits   <- c(100, 200, 300, 500)
  voc_limits   <- c(150, 250, 350, 500)
  t60_limits   <- c(1000, 4000, 8000, 10000)
  dba_limits   <- c(50, 60, 80, 100)
  lux_limits   <- c(0, 200, 400, 800)

  # need to name limits as 'val' to match up with obs dataset %>% usage
  if (target_variable == "temp") {
    val <- temp_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "hum") {
    val <- hum_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "co2") {
    val <- co2_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "pm1") {
    val <- pm1_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "pm2_5") {
    val <- pm2_5_limits[c(severity,severity+1, severity+1, severity)]
  } else if (target_variable == "pm10") {
    val <- pm10_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "hcho") {
    val <- hcho_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "nox") {
    val <- nox_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "voc") {
    val <- voc_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "t60") {
    val <- t60_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "dba") {
    val <- dba_limits[c(severity, severity+1, severity+1, severity)]
  } else if (target_variable == "lux") {
    val <- lux_limits[c(severity, severity+1, severity+1, severity)]
  }

  poly <- tibble::tibble(corner, local_time, val)

  poly

}



#' misc_values
#'
#' @description calculate number of rows of some datasets
#'
#' @param observations data.frame, the output of `wrange_observations`
#' @param devices a wrangled devices data frame as output by `wrangle_devices`
#'
#' @return a list of row counts
#'
#' @export
nrow_values <- function(devices, observations) {

  checkmate::assert_data_frame(devices)
  checkmate::assert_data_frame(observations)

  alldevice_n <- nrow(devices)
  # Calculate final numbers after removing excluded
  included_n <- nrow(devices)
  excluded_n <- alldevice_n - included_n
  # hhi_count <- n_distinct(devices$hhi, na.rm = TRUE) # update based only on included devices
  devices_rp <- observations %>% dplyr::distinct(device_id)

  list(
    alldevice_n = alldevice_n,
    included_n = included_n,
    excluded_n = excluded_n,
    devices_rp = devices_rp
  )
}





