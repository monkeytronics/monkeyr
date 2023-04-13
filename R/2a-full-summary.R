
#' date_string
#' @description Create nicely formatted sgtring from ts date
#' @param ts timestamp
#' @return Date in {mmm dd, YYYY} format
#' @export
date_string <- function(ts) {
  format(as.POSIXct(ts, origin = "1970-01-01"), "%b %d, %Y")
}


#' device_summary_string
#' @description Breif Summary of Device Connection Rate etc
#' @param ts devices_for_map : data.frame with device connection details
#' @return Character[] The text for Device Summary Section
#' @export
device_summary_string <- function(devices_for_map) {
  tryCatch (
    {
      devices_n         <- nrow(devices_for_map)
      excluded_n        <- sum(devices_for_map$connected == FALSE)
      included_n        <- sum(devices_for_map$connected == TRUE)
      unknown_hhi_n     <- sum(devices_for_map$hhi     == "unknown")
      unknown_tenure_n  <- sum(devices_for_map$tenure  == "unknown")


      ## Total number of devices
      comment_all <-
        if (devices_n == excluded_n) {
          paste("all")
        }

      ## Devices with not enough data
      comment_excluded_n <-
        if (excluded_n == 0) {
          paste("no")
        } else {
          paste(excluded_n)
        }

      ## Devices where hhi or tenure are set to "unknown"
      comment_unknown_n <-
        if ((unknown_hhi_n + unknown_tenure_n)  > 0) {
          paste(" Some values for hhi and / or tenure have not been coded. ",
            "To improve the quality of the analysis, it would be a good idea to fill these in.")
        } else {
          paste(" ")
        }



      ret <- paste0("Of the ", devices_n , " monitors included within this report, ",
      comment_all, comment_excluded_n," monitors were excluded due to having insufficient data volume within the reporting period.",
      comment_unknown_n)

      return(ret)
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "device_summary_string")
    }
  )
}


#' org_string
#' @description Define Organisation for Report Title:
#'           multiple device owners -> Event Bridge Report -> CEN Top Level
#'           single owner -> choose most common org / or independent if not filled in.
#' @param ts data_volume : data.frame with device connection details
#' @return Character[] The text for Device Summary Section
#' @export
org_string <- function(wrangled_devices, cen_top_level) {
  ## list of data source organisations : Do before exclusions. The org data is still valid nonetheless.

  tryCatch(
    {
      ## devices with valid hhi value
      devices_with_hhi <-
        wrangled_devices %>%
        filter(hhi != "unknown")

      ## create string from unique hhi values
      hhi_count <- n_distinct(devices_with_hhi$hhi, na.rm = TRUE)

      org_title <-
        if (cen_top_level == TRUE) {
          "CEN Top Level"
        } else if (hhi_count == 0) {
          "In Depth"
        } else {
          names(which.max(table(wrangled_devices$hhi)))
        }

      ## Output
      #return (org_title)

    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "org_string")
    }
  )
}



#' summary_kable_by_var
#' @description Summary Table By Target Var
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
summary_kable_by_var <- function(wrangled_devices, target_var, target_var_string, unique_owners) {
  # Test:
  # target_var = "room_type"
  tryCatch(
    {
      table_1_1 <-
        ## number of properties per hhi / tenure
        wrangled_devices %>%
        dplyr::rename(target_var := {target_var}) %>%
        dplyr::group_by(target_var) %>%
        dplyr::summarise(
          property = n_distinct(device_id),
          connected = sum(connected))

      kable_1_1 <-
        table_1_1 %>%
        kableExtra::kbl(col.names = c(target_var_string, "Number of Monitors", "Data Volume OK"),
                        format = "html",
                        align = "lcc") %>%
        kableExtra::kable_material(
          lightable_options = c("striped", "hover", "condensed"),
          html_font = "sans-serif"
          # ,full_width = TRUE  ## default value!
          # ,font_size = 14     ## Doesn't work
          ) %>%
        kableExtra::column_spec(1:3, width = c("50em","25em","25em")) %>%
        collapse_rows(columns = 1, valign = "top")

      if (nrow(table_1_1) > 6) {
        kable_1_1 <- kable_1_1 %>%
          scroll_box(height = "400px")
      }

      # Output
      kable_1_1

    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "summary_kable_by_var")
    }
  )
}


#' hhi_list_kable
#' @description Summary Table of All HHIs (CEN Report only)
#' @param devices_for_map from `get_devices_for_map()`
#' @param cen_top_level (Boolean) is this a CEN top level report
#' @return kable & description line
#' @export
hhi_list_kable <- function(devices_for_map, cen_top_level) {

  tryCatch(
    {
      if (cen_top_level) {

        ## table of HHIs
          table_1_0 <-
            devices_for_map %>%
            dplyr::mutate(connected = as.numeric(connected)) %>%
            dplyr::group_by(hhi) %>%
            dplyr::summarise(
              count     = n_distinct(device_id),
              connected = sum(connected))

        ## format into kable
          kable_1_0 <-
            table_1_0 %>%
            dplyr::relocate(connected, .after = count) %>%
            kableExtra::kbl(col.names = c("CEN Partner Org", "Number of Monitors", "Data Volume OK"),
                            format = "html",
                            align = "lcc") %>%
            kable_material(lightable_options = c("striped", "hover", "condensed"),
                           html_font = "sans-serif") %>%
            kableExtra::column_spec(1:3, width = c("50em","25em","25em"))

        ## Scroll if long
          if (nrow(table_1_0) > 8) {
            kable_1_0 <- kable_1_0 %>%
              scroll_box(height = "400px")
          }

        ## Output
          print(kable_1_0)
          cat(paste0("\n", "##### Table 1.0. CEN Partner Organisation Summary", "\n"))

      }
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "hhi_list_kable")
    }
  )
}


#' excluded_device_kable
#' @description Summary Table of Devices Excluded
#' @param devices_for_map from `get_devices_for_map()`
#' @return kable and description line
#' @export
device_excluded_kable <- function(devices_for_map) {

  tryCatch(
    {

      devices_n         <- nrow(devices_for_map)
      excluded_n        <- sum(devices_for_map$connected == FALSE)
      included_n        <- sum(devices_for_map$connected == TRUE)
      unknown_hhi_n     <- sum(devices_for_map$hhi     == "unknown")
      unknown_tenure_n  <- sum(devices_for_map$tenure  == "unknown")


      ## devices with NO data
      devices_excl <- devices_for_map %>%
        filter(connected == FALSE)

      ## Only knit if something to show
      if (nrow(devices_excl) > 0) {
        table_1_3 <-
          devices_excl %>%
          dplyr::group_by(device_id, has_coords, city, suburb) %>%
          dplyr::summarise() %>%
          arrange(`city`, `suburb`)

        kable_1_3 <-
          table_1_3 %>%
          kableExtra::kbl(col.names = c("Device Id", "Has Location Data", "City", "Suburb"),
                          format = "html",
                          align = "lccc") %>%
          kable_material(lightable_options = c("striped", "hover", "condensed"),
                         html_font = "sans-serif") %>%
          kableExtra::column_spec(1:4, width = c("25em","25em","25em","25em"))

        if (nrow(table_1_3) > 5) {
          kable_1_3 <- kable_1_3 %>%
            scroll_box(height = "400px")
        }
        ## Output
        cat(paste0("\n", "### 1.2 Monitors Excluded due to Insufficient Data"), "\n")

        cat(paste0(
          "\n\n",
          "Devices which are connected less than 10% of the time or with ",
          "fewer than 300 observations are excluded from the analysis.",
          "\n\n")
        )
        print(kable_1_3)
        cat(paste0("\n", "##### Table 1.3. Monitors Excluded Due to Insufficient Data", "\n"))
      }
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "device_excluded_kable")
    }
  )
}


