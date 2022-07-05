
#' date_string
#' @description Create nicely formatted sgtring from ts date
#' @param ts timestamp
#' @return Date in {mmm dd, YYYY} format
#' @export
date_string <- function(ts) {
  format(as.POSIXct(ts, origin = "1970-01-01"), "%b %d, %Y")
}


#' device_summary_string
#' @description Breif Summary of Device Success / Failure
#' @param ts data_volume : data.frame with device connection details
#' @return Character[] The text for Device Summary Section
#' @export
device_summary_string <- function(data_volume) {

  tryCatch(
    {
      devices_n   <- nrow(data_volume)
      excluded_n  <- sum(data_volume$exclude == 1)
      included_n  <- sum(data_volume$exclude == 0)

      comment_all <-
        if (devices_n == excluded_n) {
          paste("all")
        }

      comment_excluded_n <-
        if (excluded_n == 0) {
          paste("no")
        } else {
          paste(excluded_n)
        }

      ret <- paste0("Of the ", devices_n , " devices included within this report, ",
      comment_all, comment_excluded_n," devices were excluded due to having insufficient data volume within the reporting period.")

      return(ret)
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "device_summary_string")
    }
  )
}


#' org_string
#' @description Brief Summary of Device Success / Failure
#' @param ts data_volume : data.frame with device connection details
#' @return Character[] The text for Device Summary Section
#' @export
org_string <- function(wrangled_devices) {
  ## list of data source organisations : Do before exclusions. The org data is still valid nonetheless.

  tryCatch(
    {
      ## devices with valid hhi value
      devices_with_hhi <-
        wrangled_devices %>%
        filter(hhi != "Unknown")

      ## create string from unique hhi values
      hhi_count <- n_distinct(devices_with_hhi$hhi, na.rm = TRUE)
      list_org <-
        if (hhi_count == 0) {
          print("Independent")
        } else if (hhi_count == 1) {
          print(levels(as.factor(devices_with_hhi$hhi)))
        } else {
          print( paste( sapply(list(levels(as.factor(devices_with_hhi$hhi))[-hhi_count]), paste, collapse = ", "),
                        " and ",
                        levels(as.factor(devices_with_hhi$hhi))[hhi_count]))
        }

      ## Output
      list_org

    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "org_string")
    }
  )
}



#' home_summary_kable
#' @description Summary Table By Address
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
home_summary_kable <- function(wrangled_devices) {
  tryCatch(
    {
      table_1_1 <-
        ## number of properties per hhi / tenure
        wrangled_devices %>%
        group_by(hhi, tenure) %>%
        summarise(property = n_distinct(addr))

      kable_1_1 <-
        table_1_1 %>%
        # kable() %>%   ## kbl extends this.
        kableExtra::kbl(col.names = c("HHI Provider", "Home Tenure", "Number of Homes"),
                        format = "html",
                        align = "llc") %>%
        kableExtra::kable_material(
          lightable_options = c("striped", "hover", "condensed"),
          html_font = "sans-serif"
          # ,full_width = TRUE  ## default value!
          # ,font_size = 14     ## Doesn't work
          ) %>%
        kableExtra::column_spec(1:3, width = c("40em","40em","30em"))

      # Output
      kable_1_1

    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "home_summary_kable")
    }
  )
}


#' room_summary_kable
#' @description Summary Table By Room Type (Bedrooms vs Living)
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
room_summary_kable <- function(wrangled_devices) {

  tryCatch(
    {
      table_1_2 <-
        ## hhi / device manager | location | count

        wrangled_devices %>%
        # summary table
        group_by(hhi, device_owner, tenure, room_type) %>%
        summarise(count = n_distinct(device_id))

      kable_1_2 <-
        table_1_2 %>%
        kableExtra::kbl(col.names = c("HHI Provider", "Owner", "Home Tenure", "Room Type", "Count")) %>%
        kable_material(
          lightable_options = c("striped", "hover", "condensed"),
          html_font = "sans-serif"
        )

      ## Output
      kable_1_2

    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "room_sumary_kable")
    }
  )
}


#' excluded_device_kable
#' @description Summary Table of Devices Excluded
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
device_excluded_kable <- function(data_volume) {

  tryCatch(
    {
      ## devices with NO data
      devices_excl <- data_volume %>%
        filter(exclude == 1)

      ## Only knit if something to show
      if (nrow(devices_excl) > 0) {
        table_1_3 <-
          devices_excl %>%
          group_by(hhi, tenure, device_id) %>%
          summarise()

        kable_1_3 <-
          table_1_3 %>%
          kableExtra::kbl(col.names = c("HHI Provider", "Home Tenure", "Device Id"),
                          format = "html",
                          align = "llc") %>%
          kable_material(lightable_options = c("striped", "hover", "condensed"),
                         html_font = "sans-serif") %>%
          kableExtra::column_spec(1:3, width = c("40em","40em","30em"))

        if (nrow(table_1_3) > 5) {
          kable_1_3 <- kable_1_3 %>%
            scroll_box(height = "400px")
        }
        ## Output
        print(kable_1_3)
        cat(paste0("\n", "##### Table 1.3. Devices Excluded Due to Insufficient Data", "\n"))
      }
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "org_string")
    }
  )
}


