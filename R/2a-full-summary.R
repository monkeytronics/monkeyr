
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
}


#' get_org
#' @description Brief Summary of Device Success / Failure
#' @param ts data_volume : data.frame with device connection details
#' @return Character[] The text for Device Summary Section
#' @export
org_string <- function(wrangled_devices) {
  ## list of data source organisations : Do before exclusions. The org data is still valid nonetheless.
  hhi_count <- n_distinct(wrangled_devices$hhi, na.rm = TRUE)
  list_org <-
    if (hhi_count == 0) {
      print("Independent")
    } else if (hhi_count == 1) {
      print(levels(as.factor(wrangled_devices$hhi)))
    } else {
      print( paste( sapply(list(levels(as.factor(wrangled_devices$hhi))[-hhi_count]), paste, collapse = ", "),
                    " and ",
                    levels(as.factor(wrangled_devices$hhi))[hhi_count]))
    }
  ## Output
  list_org
}


#' home_summary_kable
#' @description Summary Table By Address
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
home_summary_kable <- function(wrangled_devices) {
  table_1_1 <-
    ## number of properties per hhi / tenure
    wrangled_devices %>%
    group_by(hhi, tenure) %>%
    summarise(property = n_distinct(addr))

  kable_1_1 <-
    table_1_1 %>%
    kable(col.names = c("HHI Provider", "Home Tenure", "Number of Homes"),format = "html", table.attr = "style='width:30%;'") %>%
    kableExtra::kable_material(
      lightable_options = c("striped", "hover", "condensed")
      ,html_font = "IBM Plex Mono"
      ,full_width = TRUE
      )

  if (nrow(table_1_1) > 0) {
    kable_1_1 <- kable_1_1 %>%
      collapse_rows(columns = 1, valign = "middle")
  }
  ## Output
  kable_1_1
}


#' room_summary_kable
#' @description Summary Table By Room Type (Bedrooms vs Living)
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
room_summary_kable <- function(wrangled_devices) {
  table_1_2 <-
    ## hhi / device manager | location | count

    wrangled_devices %>%
    # summary table
    group_by(hhi, device_owner, tenure, room_type) %>%
    summarise(count = n_distinct(device_id))

  kable_1_2 <-
    table_1_2 %>%
    kable(col.names = c("HHI provider", "Device Manager", "Home Tenure", "Room Type", "Count")) %>%
    kable_material(
      lightable_options = c("striped", "hover", "condensed", "responsive")
      ,html_font = "IBM Plex Mono"
      )

  if (nrow(table_1_2) > 0) {
    kable_1_2 <- kable_1_2 %>%
      collapse_rows(columns = 1:3, valign = "middle")
  }
  ## Output
  kable_1_2
}


#' excluded_device_kable
#' @description Summary Table of Devices Excluded
#' @param ts wrangled_devices : data.frame device table!
#' @return kable
#' @export
device_excluded_kable <- function(data_volume) {

  devices_excl <- data_volume %>%
    filter(tenure == "unknown")
    # filter(exclude == 1)

  if (nrow(devices_excl) > 0) {
  # if (TRUE) {
    table_1_3 <-
      devices_excl %>%
      group_by(hhi, tenure, device_id) %>%
      summarise()

    kable_1_3 <-
      table_1_3 %>%
      kable(col.names = c("HHI Provider", "Home Tenure", "Device Id")) %>%
      kable_material(lightable_options = c("striped", "hover", "condensed"),
                     html_font = "IBM Plex Mono")

    if (nrow(table_1_3) > 0) {
      kable_1_3 <- kable_1_3 %>%
        collapse_rows(columns = 1, valign = "middle")
    }
    ## Output
    kable_1_3
  }
}


