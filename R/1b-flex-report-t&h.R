#' set_options : Need to update this to work with tidy data structure
#' @description Set options for downstream charts based on available data
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @export
set_options <- function(observations) {
  checkmate::assert_data_frame(observations)

  options(TEMP_ENABLE  = "temp"  %in% colnames(observations))
  options(HUM_ENABLE   = "hum"   %in% colnames(observations))
  options(CO2_ENABLE   = "co2"   %in% colnames(observations))
  options(LUX_ENABLE   = "lux"   %in% colnames(observations))
  options(DBA_ENABLE   = "dba"   %in% colnames(observations))
  options(VOC_ENABLE   = "voc"   %in% colnames(observations))
  options(HCHO_ENABLE  = "hcho"  %in% colnames(observations))
  options(PM1_ENABLE   = "pm1"   %in% colnames(observations))
  options(PM2_5_ENABLE = "pm2_5" %in% colnames(observations))
  options(PM10_ENABLE  = "pm10"  %in% colnames(observations))

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
  options(TEMP_COLOURS = unikn::usecol(pal = pal_seeblau, n = nrow(devices)))
  options(HUM_COLOURS  = unikn::usecol(pal = pal_pinky, n = nrow(devices)))
  options(CO2_COLOURS  = unikn::usecol(pal = pal_seegruen, n = nrow(devices)))
}

#' ts_chart
#' @description make a ggplotly time series chart for temperature, CO2, ...
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @param from_timetamp,to_timestamp timestamps for the begin and end of the report
#' @param target_variable character, the name of the column to use for plotting
#' Options are (for now): `temp`, `hum`
#' @examples
#' ts_chart(
#'   observations = wrangled_obs,
#'   from_timestamp = test_params()$fromTimeStamp,
#'   to_timestamp = test_params()$toTimeStamp,
#'   target_variable = "hum"
#' )
#' @export
ts_chart <-
  function(observations,
           from_timestamp,
           to_timestamp,
           target_variable) {
    checkmate::assert_number(from_timestamp)
    checkmate::assert_number(to_timestamp)
    checkmate::assert_data_frame(observations)

    blue_zone_1 <- make_polygon_area(observations, target_variable, severity = 1)
    blue_zone_2 <- make_polygon_area(observations, target_variable, severity = 2)
    blue_zone_3 <- make_polygon_area(observations, target_variable, severity = 3)


    report_period <- report_period(from_timestamp , to_timestamp)
    target_colours <-
      getOption(paste0(toupper(target_variable), "_COLOURS"))

    y_lim <- switch(
      target_variable,
      "temp" = c(0, 30),
      "hum"  = c(30, 100),
      "co2"  = c(250, 2000)
    )

    y_lab <- switch(
      target_variable,
      "temp" = "Temperature (°C)",
      "hum"  = "Humidity (%)",
      "co2"  = "CO2 (ppm)"
    )

    t_lab <- switch(
      target_variable,
      "temp" = "Temperature Time Series ",
      "hum"  = "Rel. Humidity Time Series ",
      "co2"  = "Carbon Dioxide Time Series "
    )

    observations <- observations %>%
      dplyr::filter(reading == target_variable & !is.na(ts))

    axis_high <- max(y_lim[[2]], max(as.numeric(observations$val), na.rm = TRUE))
    axis_low  <- min(y_lim[[1]],  min(as.numeric(observations$val), na.rm = TRUE))
    observations
    ts_chart <- observations %>%
      ggplot2::ggplot(ggplot2::aes(
        x = local_time,
        y = val,
        colour = device_id
      )) +
      ggplot2::geom_polygon(data = blue_zone_1, colour = "white", alpha = 0.20, fill = "royalblue") +
      ggplot2::geom_polygon(data = blue_zone_2, colour = "white", alpha = 0.30, fill = "royalblue") +
      ggplot2::geom_polygon(data = blue_zone_3, colour = "white", alpha = 0.40, fill = "royalblue") +
      ggplot2::geom_line() +
      ggplot2::scale_color_manual(values = target_colours) +
      ggplot2::labs(title = paste0(t_lab, report_period),
                    colour = "Room") +
      ggplot2::ylab(y_lab) +
      ggplot2::xlab("Change_Date") +
      ggplot2::ylim(axis_low, axis_high) +
      hrbrthemes::theme_ipsum() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = 10,
          angle = 60,
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 10),
        plot.title  = ggplot2::element_text(size = 13),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 11),
        legend.text = ggplot2::element_text(size = 10)
      )

    # Turn it interactive with ggplotly
    plotly::ggplotly(ts_chart, tooltip = c("x", "y", "colour")) %>%
    plotly::style(hoverinfo = "skip", traces = 1)
  }


#' get_values_for_boxes
#' @description get a list of summary stats for value boxes
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @param target_variable character, the name of the column to use for summary
#' @export
get_extreme_vals <- function(observations, target_variable) {
  checkmate::assert_data_frame(observations)
  checkmate::test_character(target_variable)
  obs <- observations %>%
    dplyr::filter(reading == target_variable & !is.na(ts))
  list(
    highest_val = max(as.numeric(obs$val), na.rm = TRUE),
    lowest_val  = min(as.numeric(obs$val), na.rm = TRUE),
    mean_val    = mean(as.numeric(obs$val), na.rm = TRUE) %>% round(digits = 1)
  )
}
