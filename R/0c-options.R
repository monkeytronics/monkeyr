#' set_options : Need to update this to work with tidy data structure
#' @description Set options for downstream charts based on available data
#' @param wrangled_obs data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @export
set_options <- function(wrangled_obs) {
  checkmate::assert_data_frame(wrangled_obs)

  unique_readings <- monkeyr::unique_readings(wrangled_obs)
  for (i in 1 : length(unique_readings)) {
    options(setNames(list(TRUE), unique_readings[i]))
  }
  # getOption(unique_readings[i])
}


#' unique_readings
#' @description returns the unique readings available in dataset
#' @param wrangled_obs data.frame, as output by `wrangle_observations`
#' @export
unique_readings <- function(wrangled_obs) {
  as.vector(wrangled_obs$reading) %>%
    unique()
}


#' valid_readings
#' @description returns the list of permitted readings types
#' @export
permitted_readings <- function() {
  permitted_readings <- c(
    "temp",  "hum",   "co2"         # std
    ,"voc",   "nox",   "hcho"        # chem
    ,"pm1",   "pm2_5", "pm10"        # PM
    ,"dba",   "t60",   "lux"          # schools
  )
}


#' filter_list
#' @description returns the valid readings. Of those available, it filters against
#' the list of permitted values - stops propogation of unexpected values.
#' @param available list, readings we have in our dataset
#' @param permitted list, list of permitted readings types
#' @export
filter_list <- function(available, permitted) {
  # available <- c("dba", "fdasfdas", "fdasfdasfas")
  # permitted <- c("dba", "temp", "hum")
  tib <- as_tibble(available) %>%
    filter(value %in% permitted)
  return (tib$value)
}


#' param_list
#' @description returns report params as a list
#' @param report_params The string with '-' separation
#' @export
param_list <- function(report_params) {
  # report_params <- "-map-tenure-room-roomType-"
  param_list <- as.list(strsplit(report_params, "-")[1])
  param_list <- lapply(param_list, function(x) {x[!x == ""]})
  param_list <- param_list[[1]]
}


#' target_var_list
#' @description matches requested report params to available segments for reporting
#' @param wrangled_obs data.frame, as output by `wrangle_observations`
#' @param param_list list, report parameters from param_list
#' @export
target_var_list <- function(wrangled_obs, param_list) {
  # param_list <- c("map", "room", "room_type")
  tib <- as_tibble(colnames(wrangled_obs)) %>%
    filter(value %in% param_list)
  return (tib$value)
}


#' monkey_palettes
#' @description define some color palettes and set them as R options to be globally available
#' @param devices data.frame, the wrangled devices data as output by `wrangle_devices`,
#' and further filtered by `remove_excluded_devices`
#' @export
monkey_palettes <- function(devices) {
  checkmate::assert_data_frame(devices)
  ## Colors with unikn package
  # https://cran.r-project.org/web/packages/unikn/vignettes/colors.html
  options(TEMP_COLOURS   = unikn::usecol(pal = pal_unikn_dark,  n = nrow(devices)))
  options(HUM_COLOURS    = unikn::usecol(pal = pal_karpfenblau, n = nrow(devices)))
  options(CO2_COLOURS    = unikn::usecol(pal = pal_bordeaux,    n = nrow(devices)))
  options(PM1_COLOURS    = unikn::usecol(pal = pal_seegruen,    n = nrow(devices)))
  options(PM2_5_COLOURS  = unikn::usecol(pal = pal_petrol,      n = nrow(devices)))
  options(PM10_COLOURS   = unikn::usecol(pal = pal_karpfenblau, n = nrow(devices)))
  options(HCHO_COLOURS   = unikn::usecol(pal = pal_grau,        n = nrow(devices)))
  options(DBA_COLOURS    = unikn::usecol(pal = pal_petrol,      n = nrow(devices)))
  options(LUX_COLOURS    = unikn::usecol(pal = pal_seegruen,    n = nrow(devices)))
  options(VOC_COLOURS    = unikn::usecol(pal = pal_unikn_dark,  n = nrow(devices)))
  options(T60_COLOURS    = unikn::usecol(pal = pal_unikn_dark,  n = nrow(devices)))
  options(DBA_COLOURS    = unikn::usecol(pal = pal_unikn_dark,  n = nrow(devices)))
  options(LUX_COLOURS    = unikn::usecol(pal = pal_unikn_dark,  n = nrow(devices)))
}
