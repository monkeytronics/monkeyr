#' conn_wrangle
#' @description Connectivity Wrangle - Primary table is devices : not obs
#' @param ts wrangled_devices : data.frame device table
#' @param ts wrangled_obs     : data.frame obs table
#'
#' @return wrangled_devices Data Frame Joined with Obs
#' @export
conn_wrangle <- function(wrangled_devices, filtered_obs) {

  checkmate::assert_data_frame(wrangled_devices)

  if (checkmate::test_character(filtered_obs)) {
    wrangled_obs <-
      readr::read_csv(filtered_obs, col_types = "ciiicd")
  } else {
    wrangled_obs <- filtered_obs
  }

  tryCatch(
    {
      wrangled_devices <-
        wrangled_devices %>%
        dplyr::select(c(school_name = school, city, device_id, country, classRoom, long, lat)) %>%

        ## Join obs data
        dplyr::full_join(
          dplyr::select(
            wrangled_obs
          ),
          by = "device_id"
        ) %>%
        dplyr::filter(reading == "temp") %>%
        dplyr::select(-reading)

    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "conn_wrangle")
    }
  )
}



#' conn_summary
#' @description Connectivity Summary Kable
#' @param ts wrangled_devices : data.frame
#'
#' @return wrangled_devices Data Frame Full Joined with Obs
#' @export
conn_summary <- function(wrangled_devices) {

  # summarise by device
    # get latest ts
    # new col = online
  # summarise by school
    # total devices & online
    # new col %

  tryCatch(
    {

      ## How many records should there be?
      interval <- 15 * 60
      timespan <- params$toTimeStamp - params$fromTimeStamp
      fullvol  <- (timespan / interval) + 1

      table_conn_summary <-
        wrangled_devices %>%
        dplyr::group_by(across(c(-seq, -rssi, -ts, -country))) %>%
        dplyr::summarise(
          last_conn = max(ts),
          data_vol = n()
          ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          last_conn = tidyr::replace_na(last_conn, 0)
        ) %>%
        dplyr::select(-c(device_id))

      ## kable
      kable_1_1 <-
        table_1_1 %>%
        kableExtra::kbl()

    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "conn_summary")
    }
  )
}



