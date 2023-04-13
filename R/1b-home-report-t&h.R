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
ts_chart <- function(observations,
                     from_timestamp,
                     to_timestamp,
                     target_variable)
{
  checkmate::assert_number(from_timestamp)
  checkmate::assert_number(to_timestamp)
  checkmate::assert_data_frame(observations)

  ## Test
  ## observations = wrangled_obs
  ## from_timestamp = params$fromTimeStamp
  ## to_timestamp = params$toTimeStamp
  ## target_variable = "temp"

  tryCatch (
    {
      ## Make Polygons for target var
      blue_zone_1 <- make_polygon_area(observations, target_variable, severity = 1)
      blue_zone_2 <- make_polygon_area(observations, target_variable, severity = 2)
      blue_zone_3 <- make_polygon_area(observations, target_variable, severity = 3)




      y_lim <- switch(
        target_variable,
        "temp"  = c(0, 30),
        "hum"   = c(30, 100),
        "co2"   = c(250, 2000),
        "pm1"   = c(0, 50),
        "pm2_5" = c(0, 50),
        "pm10"  = c(0, 50),
        "hcho"  = c(0, 100),
        "nox"   = c(0, 500),
        "voc"   = c(0, 500),
        "lux"   = c(0, 1000),
        "t60"   = c(0, 10000),
        "dba"   = c(25, 100),
      )

      y_lab <- switch(
        target_variable,
        "temp"  = "Temperature (°C)",
        "hum"   = "Humidity (%)",
        "co2"   = "CO2 (ppm)",
        "pm1"   = "PM 1 (ppm)",
        "pm2_5" = "PM 2.5 (ppm)",
        "pm10"  = "PM 10 (ppm)",
        "hcho"  = "Formaldehyde (ppb)",
        "nox"   = "NOx (Index)",
        "voc"   = "VOC (Index)",
        "lux"   = "Lighting Level (Lux)",
        "t60"   = "T60 Reverberation (ms)",
        "dba"   = "Sound Pressure Level (dB A-Weighted)",
      )

      t_lab <- switch(
        target_variable,
        "temp"  = "Temperature Time Series ",
        "hum"   = "Rel. Humidity Time Series ",
        "co2"   = "Carbon Dioxide Time Series ",
        "pm1"   = "PM 1 Time Series ",
        "pm2_5" = "PM 2.5 Time Series ",
        "pm10"  = "PM 10 Time Series ",
        "hcho"  = "Formldehyde Time Series ",
        "nox"   = "Nitrogen Dioxide Time Series ",
        "voc"   = "VOC Time Series ",
        "lux"   = "Lighting Level Time Series ",
        "t60"   = "T60 Reverb Time Series ",
        "dba"   = "Sound Pressure Level Time Series ",
      )


      ## prep data set
      observations <- observations %>%
        dplyr::filter(reading == target_variable & !is.na(ts)) %>%

      ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) > 0,
          paste0(device_id, " - ", name),
          name
        ))


      ## Set colours & title
      report_period <- report_period(from_timestamp , to_timestamp)
      # target_colours <- getOption(paste0(toupper(target_variable), "_COLOURS"))
      target_colours <- get_palette(toupper(target_variable), length(unique(observations$name)))


      axis_high <- max(y_lim[[2]], max(as.numeric(observations$val), na.rm = TRUE))
      axis_low  <- min(y_lim[[1]],  min(as.numeric(observations$val), na.rm = TRUE))
      # observations
      ts_chart <- observations %>%
        ggplot2::ggplot(ggplot2::aes(
          x = local_time,
          y = val,
          colour = name
        )) +
        ggplot2::geom_polygon(data = blue_zone_1, colour = "white", alpha = 0.20, fill = "royalblue") +
        ggplot2::geom_polygon(data = blue_zone_2, colour = "white", alpha = 0.30, fill = "royalblue") +
        ggplot2::geom_polygon(data = blue_zone_3, colour = "white", alpha = 0.40, fill = "royalblue") +
        ggplot2::geom_line() +

        ggplot2::scale_color_manual(values = target_colours) +
        ggplot2::labs(title = paste0(t_lab, report_period)) +
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

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = paste0("ts_chart - ", target_variable))
    }
  )
}



#' get_values_for_boxes
#' @description get a list of summary stats for value boxes
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle_observations` and further filtered by `remove_excluded_devices`
#' @param target_variable character, the name of the column to use for summary
#' @export
get_extreme_vals <- function(observations, target_variable) {
  tryCatch (
    {
      checkmate::assert_data_frame(observations)
      checkmate::test_character(target_variable)
      obs <- observations %>%
        dplyr::filter(reading == target_variable & !is.na(ts))
      list(
        highest_val = max(as.numeric(obs$val), na.rm = TRUE),
        lowest_val  = min(as.numeric(obs$val), na.rm = TRUE),
        mean_val    = mean(as.numeric(obs$val), na.rm = TRUE) %>% round(digits = 1)
      )

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = paste0("get_extreme_vals - ", target_variable))
    }
  )
}
