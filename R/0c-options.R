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
param_list <- function(report_params, split_char = ",") {
  # report_params <- "-map-tenure-room-roomType-"
  param_list <- as.list(strsplit(report_params, split_char)[1])
  param_list <- lapply(param_list, function(x) {x[!x == ""]})
  param_list <- param_list[[1]]
}


#' target_var_list
#' @description matches requested report params to available segments for reporting
#' @param wrangled_devices data.frame, as output by `wrangled_devices`
#' @param param_list list, report parameters from param_list. The ones we want will take the form
#' '{param} anlys' - for instance 'room_type anlys'
#' @export
target_var_list <- function(wrangled_devices, param_list) {
  # param_list <- c("show map deloyment", "room anlys", "room_type anlys")

  # remove " anlys" junk
  param_list <- gsub(" anlys", "", param_list)

  # return matching terms as tibble
  tib <- as_tibble(colnames(wrangled_devices)) %>%
    filter(value %in% param_list)

  monkey_knit_msg(msg = paste0("target_var_list = ", tib$value), resource="target_var_list")
  return (tib$value)
}

#' target_group_list
#' @description captures requested report group params : to compare to device group as reporting segment
#' @param wrangled_devices data.frame, as output by `wrangle_devices`
#' @param param_list list, report parameters from param_list. The ones we want will take the form
#' '{param} anlys' - for instance 'room_type anlys'
#' @export
target_group_list <- function(wrangled_devices, param_list) {
  # param_list <- c("Analyse My Cool Project group", "Analyse YYYY group", "Analyse ZZZZ group")
  # groups are in wrangled_devices$cluster as 'ZZZZ-YYYY-My Cool Project-OtherGroups'

  # Get valid groups from wrangled devices
  groups_valid <-
    wrangled_devices$cluster %>%
    na.omit() %>%
    unique() %>%
    lapply(function(x) {param_list(x, "-")}) %>%
    unlist()

  # groups requested into tibbble
  tib <-
    tibble(groups_req = param_list) %>%
    filter(stringr::str_starts(groups_req, "Analyse")) %>%
    filter(stringr::str_ends(groups_req, "group")) %>%

    # only keep valid ones
    filter(groups_req %in% groups_valid)

  monkey_knit_msg(msg = paste0("target_group_list = ", tib$groups_req), resource="target_group_list")
  return (tib$groups_req)
}


#' data_segments
#' @description work out the max number of data segments - useful for formatting plots
#' @param wrangled_devices data.frame, as output by `wrangle_devices`
#' @param target_var_list list, report parameters which we will segment upon
#' @export
data_segments   <- function(wrangled_devices, target_var_list) {
  max_segments <- 0
  if (length(target_var_list) == 0) {
    retrurn (0)
  } else {
    for (index in 1:length(target_var_list)) {
      target_var <- target_var_list[index]
      unique_segments <-
        as.vector(wrangled_obs[[target_var]]) %>%
        unique() %>%
        length()

      max_segments <- max(
        max_segments,
        unique_segments
      )
    }
  }
  ## output
  monkey_knit_msg(msg = paste0("max_segments = ", max_segments), resource="data_segments")
  return(max_segments)
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
